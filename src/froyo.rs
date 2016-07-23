// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow;
use std::path::Path;
use std::cmp::{Ordering, max};
use std::io;
use std::io::ErrorKind;
use std::error::Error;

use uuid::Uuid;
use devicemapper::DM;
use serde_json;
use time;
use bytesize::ByteSize;

use blockdev::{BlockDev, BlockDevs, BlockMember};
use blockdev::LinearSegment;
use raid::{RaidDevs, RaidSegment, RaidLinearDev, RaidStatus,
           RaidAction, RaidMember, RaidLayer};
use thin::{ThinPoolDev, ThinPoolStatus, ThinPoolWorkingStatus};
use thin::{ThinDev, ThinStatus};
use mirror::{MirrorDev, TempDev, TempLayer};
use types::{Sectors, SectorOffset, DataBlocks, FroyoError, FroyoResult, InternalError};
use dbus_api::DbusContext;
use util::short_id;
use consts::*;

pub use serialize::FroyoSave;

#[derive(Debug, Clone)]
pub struct Froyo<'a> {
    pub id: String,
    pub name: String,
    pub block_devs: BlockDevs,
    raid_devs: RaidDevs,
    thin_pool_dev: ThinPoolDev,
    thin_devs: Vec<ThinDev>,
    throttled: bool,
    last_state: FroyoState,
    pub dbus_context: Option<DbusContext<'a>>,
}

#[derive(Debug, Clone)]
pub enum FroyoState {
    Initializing,
    Good(FroyoRunningState),
    RaidFailed,
    ThinPoolFailed,
    ThinFailed,
}

impl FroyoState {
    pub fn is_reshaping(&self) -> bool {
        match *self {
            FroyoState::Good(FroyoRunningState::Reshaping(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum FroyoRunningState {
    Good,
    Reshaping(ReshapeState),
    Degraded(u8),
}

#[derive(Debug, Clone)]
pub enum ReshapeState {
    Off,
    Idle,
    CopyingToRaid(MirrorDev),
    CopyingToScratch(MirrorDev),
    CopyingFromScratch(MirrorDev),
    SyncingRaids,
}

impl ReshapeState {
    pub fn is_busy(&self) -> bool {
        match *self {
            ReshapeState::Off => panic!("should never happen"),
            ReshapeState::Idle => false,
            _ => true,
        }
    }
}

impl<'a> Froyo<'a> {
    pub fn new<T>(name: &str, paths: &[T], force: bool)
                     -> FroyoResult<Froyo<'a>>
        where T: borrow::Borrow<Path>
    {
        if paths.len() < MIN_BLK_DEVS {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput, "At least 2 block devices must be given")))
        }

        if paths.len() > MAX_BLK_DEVS {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!("Max supported devices is {}, {} given",
                        MAX_BLK_DEVS, paths.len()))))
        }

        let froyo_id = Uuid::new_v4().to_simple_string();
        let mut block_devs = BlockDevs(BTreeMap::new());
        for path in paths {
            let bd = try!(BlockDev::new(&froyo_id, path.borrow(), force));
            block_devs.0.insert(bd.id.clone(),
                              BlockMember::Present(Rc::new(RefCell::new(bd))));
        }

        let dm = try!(DM::new());

        let raid_devs = try!(RaidDevs::new(&dm, &froyo_id, &block_devs));

        let meta_size = TPOOL_INITIAL_META_SECTORS;
        let data_size = TPOOL_INITIAL_DATA_SECTORS;

        let meta_raid_segments = try!(
            raid_devs.alloc_raid_segments(meta_size)
                .ok_or_else(||
                            io::Error::new(io::ErrorKind::InvalidInput,
                                           "no space for thinpool meta")));
        let data_raid_segments = try!(
            raid_devs.alloc_raid_segments(data_size)
                .ok_or_else(||
                            io::Error::new(io::ErrorKind::InvalidInput,
                                           "no space for thinpool data")));
        let thin_pool_dev = try!(ThinPoolDev::new(
            &dm, &froyo_id, meta_raid_segments, data_raid_segments));

        Ok(Froyo {
            name: name.to_owned(),
            id: froyo_id,
            block_devs: block_devs,
            raid_devs: raid_devs,
            thin_pool_dev: thin_pool_dev,
            thin_devs: Vec::new(),
            throttled: false,
            last_state: FroyoState::Initializing,
            dbus_context: None,
        })
    }

    fn initial_resync_complete(&mut self) -> FroyoResult<()> {
        let dm = try!(DM::new());

        dbgp!("initial resync complete, creating thin dev");

        // Create an initial 1TB thin dev
        self.thin_devs.push(try!(ThinDev::new(
            &dm,
            &self.id,
            &self.name, // 1st thindev name same as froyodev name
            0,
            THIN_INITIAL_SECTORS,
            &self.thin_pool_dev)));

        self.last_state = FroyoState::Good(FroyoRunningState::Good);

        try!(self.save_state());

        Ok(())
    }

    fn to_save(&self) -> FroyoSave {
        FroyoSave {
            name: self.name.to_owned(),
            id: self.id.to_owned(),
            block_devs: self.block_devs.to_save(),
            raid_devs: self.raid_devs.raids.iter()
                .map(|(id, rd)| (id.clone(), rd.borrow().to_save()))
                .collect(),
            temp_dev: self.raid_devs.temp_dev.as_ref()
                .map(|ref td| td.borrow().to_save()),
            thin_pool_dev: self.thin_pool_dev.to_save(),
            thin_devs: self.thin_devs.iter()
                .map(|x| x.to_save())
                .collect(),
        }
    }

    pub fn destroy(&mut self) -> FroyoResult<()> {
        try!(self.teardown());
        try!(self.block_devs.wipe());

        Ok(())
    }

    pub fn to_metadata(&self) -> FroyoResult<String> {
        Ok(try!(serde_json::to_string(&self.to_save())))
    }

    pub fn to_metadata_pretty(&self) -> FroyoResult<String> {
        Ok(try!(serde_json::to_string_pretty(&self.to_save())))
    }

    pub fn find_all() -> FroyoResult<Vec<Froyo<'a>>> {
        // We could have BlockDevs for multiple Froyodevs.
        // Group them by Froyo uuid.
        let mut froyo_devs = BTreeMap::new();
        for bd in try!(BlockDev::find_all()) {
            froyo_devs.entry(bd.froyodev_id.clone())
                .or_insert_with(Vec::new)
                .push(bd);
        }

        let mut froyos = Vec::new();
        for (froyo_id, bds) in froyo_devs {
            let buf = {
                // get newest metadata across all blockdevs and in either MDA
                let newest_bd = bds.iter()
                    .map(|bd| {
                        let mda = match bd.mdaa.last_updated.cmp(&bd.mdab.last_updated) {
                            Ordering::Less => &bd.mdab,
                            Ordering::Greater => &bd.mdaa,
                            Ordering::Equal => &bd.mdab,
                        };
                        (mda.last_updated, bd)
                    })
                    .max_by_key(|&(tm, _)| tm)
                    .unwrap().1;
                try!(newest_bd.read_mdax())
            };
            let s = String::from_utf8_lossy(&buf).into_owned();

            let froyo_save = try!(serde_json::from_str::<FroyoSave>(&s));

            match Froyo::setup(&froyo_save, froyo_id, bds) {
                Ok(f) => froyos.push(f),
                Err(e) => dbgp!("Error: {}", e.description()),
            }
        }

        Ok(froyos)
    }

    pub fn find(name: &str) -> FroyoResult<Option<Froyo>> {
        let froyos = try!(Froyo::find_all());
        for f in froyos {
            if name == f.name {
                return Ok(Some(f))
            }
        }

        Ok(None)
    }

    fn setup_blockdevs(
        froyo_save: &FroyoSave,
        found_block_devs: Vec<BlockDev>)
        -> FroyoResult<BlockDevs> {
        let mut bd_map = found_block_devs.into_iter()
            .map(|x| (x.id.clone(), x))
            .collect::<BTreeMap<_, _>>();

        let mut block_devs = BTreeMap::new();
        for (id, sbd) in &froyo_save.block_devs {
            match bd_map.remove(id) {
                Some(bd) => {
                    block_devs.insert(
                        id.clone(), BlockMember::Present(Rc::new(RefCell::new(bd))));
                },
                None => {
                    dbgp!("missing a blockdev: id {} path {}", id, sbd.path.display());
                    block_devs.insert(id.clone(), BlockMember::Absent(sbd.clone()));
                }
            }
        }

        for bd in bd_map.values() {
            dbgp!("{} header indicates it's part of {} but \
                   not found in metadata, ignoring",
                  bd.path.display(), froyo_save.name);
        }

        Ok(BlockDevs(block_devs))
    }

    fn setup_thinpool(
        dm: &DM,
        froyo_save: &FroyoSave,
        raid_devs: &RaidDevs)
        -> FroyoResult<ThinPoolDev> {
        let tpd = &froyo_save.thin_pool_dev;

        let meta_name = format!("thin-meta-{}", froyo_save.id);
        let mut raid_segments = Vec::new();
        for seg in &tpd.meta_dev.segments {
            let raid_seg = try!(raid_devs.lookup_segment(
                &seg.parent, seg.start, seg.length).ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidInput,
                                   "Could not find meta's parent")}));
            raid_segments.push(raid_seg);
        }

        let meta_raid_dev = try!(RaidLinearDev::setup(
            &dm,
            &meta_name,
            &tpd.meta_dev.id,
            raid_segments));

        let data_name = format!("thin-data-{}", froyo_save.id);
        let mut raid_segments = Vec::new();
        for seg in &tpd.data_dev.segments {
            let raid_seg = try!(raid_devs.lookup_segment(
                &seg.parent, seg.start, seg.length).ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidInput,
                                   "Could not find data's parent")}));
            raid_segments.push(raid_seg);
        }

        let data_raid_dev = try!(RaidLinearDev::setup(
            &dm,
            &data_name,
            &tpd.data_dev.id,
            raid_segments));

        ThinPoolDev::setup(
            dm,
            &froyo_save.id,
            tpd.data_block_size,
            tpd.low_water_blocks,
            meta_raid_dev,
            data_raid_dev)
    }

    fn setup(froyo_save: &FroyoSave, froyo_id: String, found_blockdevs: Vec<BlockDev>)
                 -> FroyoResult<Froyo<'a>> {
        let block_devs = try!(Froyo::setup_blockdevs(&froyo_save, found_blockdevs));

        let dm = try!(DM::new());

        let raid_devs = try!(RaidDevs::setup(&dm, &froyo_save, &block_devs));

        let thin_pool_dev = try!(Froyo::setup_thinpool(&dm, &froyo_save, &raid_devs));

        let mut thin_devs = Vec::new();
        for std in &froyo_save.thin_devs {
            thin_devs.push(try!(ThinDev::setup(
                &dm,
                &froyo_save.id,
                &std.name,
                std.thin_number,
                std.size,
                &thin_pool_dev)));
        }

        let mut froyo = Froyo {
            name: froyo_save.name.to_owned(),
            id: froyo_id.to_owned(),
            block_devs: block_devs,
            raid_devs: raid_devs,
            thin_pool_dev: thin_pool_dev,
            thin_devs: thin_devs,
            throttled: false,
            last_state: FroyoState::Good(FroyoRunningState::Good),
            dbus_context: None,
        };

        try!(froyo.check_state());

        Ok(froyo)
    }

    pub fn teardown(&mut self) -> FroyoResult<()> {
        let dm = try!(DM::new());

        for thin in &mut self.thin_devs {
            try!(thin.teardown(&dm))
        }

        try!(self.thin_pool_dev.teardown(&dm));

        try!(self.raid_devs.teardown(&dm));

        Ok(())
    }

    pub fn save_state(&self) -> FroyoResult<()> {
        let metadata = try!(self.to_metadata());
        let current_time = time::now().to_timespec();

        for bd in self.block_devs.0.values() {
            if let Some(bd) = bd.present() {
                try!(bd.borrow_mut().save_state(&current_time, metadata.as_bytes()))
            }
        }

        Ok(())
    }

    pub fn status(&self) -> FroyoResult<FroyoState> {
        let mut degraded = 0;
        for rd in self.raid_devs.raids.values() {
            let (r_status, r_action) = try!(rd.borrow().status());
            match r_status {
                RaidStatus::Failed => return Ok(FroyoState::RaidFailed),
                RaidStatus::Degraded(x) => degraded = max(degraded, x as u8),
                RaidStatus::Good => if let RaidAction::Resync = r_action {
                    if let FroyoState::Initializing = self.last_state {
                        return Ok(FroyoState::Initializing)
                    }
                },
            }
        }

        if let ThinPoolStatus::Fail = try!(self.thin_pool_dev.status()) {
            return Ok(FroyoState::ThinPoolFailed)
        }

        if let Some(td) = self.thin_devs.get(0) {
            if let ThinStatus::Fail = try!(td.status()) {
                return Ok(FroyoState::ThinFailed)
            }
        }

        // If reshaping, report that instead of the fact that some
        // raid reports degraded
        let cur_running_state = match self.last_state {
            FroyoState::Good(ref running_state) =>  {
                match running_state {
                    s @ &FroyoRunningState::Reshaping(_) => s.clone(),
                    _ => {
                        if degraded > 0 {
                            FroyoRunningState::Degraded(degraded)
                        } else {
                            FroyoRunningState::Good
                        }
                    },
                }
            },
            FroyoState::Initializing => FroyoRunningState::Good,
            _ => panic!("last_state should always transition to Good(_)"),
        };

        Ok(FroyoState::Good(cur_running_state))
    }

    // Get how much RAIDed space there is available. This consists of
    // unused blocks in the thin pool data area, and we also include
    // unused RAID space, since we could extend the data area if
    // needed. (The reason we don't is that currently there is no way
    // to shrink the size of the data area, so keeping it unallocated
    // if possible may give us more leeway in reshaping smaller.)
    //
    pub fn avail_redundant_space(&self) -> FroyoResult<Sectors> {
        let raid_avail = self.raid_devs.avail_space();

        let thinpool_avail = match try!(self.thin_pool_dev.status()) {
            ThinPoolStatus::Good((_, usage)) => {
                self.blocks_to_sectors(usage.total_data - usage.used_data)
            },
            ThinPoolStatus::Fail => {
                return Err(FroyoError::Io(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Can't get free blocks from a failed thin pool dev")))
            },
        };

        Ok(raid_avail + thinpool_avail)
    }

    pub fn data_block_size(&self) -> u64 {
        self.thin_pool_dev.data_block_size()
    }

    pub fn extend_thinpool_data_dev(&mut self, length: Sectors) -> FroyoResult<()> {
        let new_segs = try!(
            self.raid_devs.alloc_raid_segments(length)
                .ok_or_else(||
                            io::Error::new(io::ErrorKind::InvalidInput,
                                           "no space for extending thinpool data")));

        dbgp!("Extending tpool data dev by {}", *length);
        try!(self.thin_pool_dev.extend_data_dev(new_segs));

        Ok(())
    }

    pub fn extend_thinpool_meta_dev(&mut self, length: Sectors) -> FroyoResult<()> {
        let new_segs = try!(
            self.raid_devs.alloc_raid_segments(length)
                .ok_or_else(||
                            io::Error::new(io::ErrorKind::InvalidInput,
                                           "no space for extending thinpool meta")));

        dbgp!("Extending tpool meta dev by {}", *length);
        try!(self.thin_pool_dev.extend_meta_dev(new_segs));

        Ok(())
    }

    pub fn extend_thin_dev(&mut self, length: Sectors) -> FroyoResult<()> {
        self.thin_devs[0].extend(length)
    }

    pub fn blocks_to_sectors(&self, blocks: DataBlocks) -> Sectors {
        self.thin_pool_dev.blocks_to_sectors(blocks)
    }

    pub fn sectors_to_blocks(&self, sectors: Sectors) -> DataBlocks {
        self.thin_pool_dev.sectors_to_blocks(sectors)
    }

    pub fn update_dbus(&self) -> FroyoResult<()> {
        if let Some(ref dc) = self.dbus_context {
            let remaining = try!(self.avail_redundant_space());
            try!(DbusContext::update_one(&dc.remaining_prop, (*remaining).into()));
            let total = self.raid_devs.total_space();
            try!(DbusContext::update_one(&dc.total_prop, (*total).into()));


            // TODO: self.status() is not returning all status we can
            // report via dbus, and they're also mutually exclusive,
            // but dbus is not. If we inline that in this fn, that
            // might make this easier to achieve.
            let (status, mut r_status): (u32,u32) = match try!(self.status()) {
                FroyoState::RaidFailed => (0x100, 0),
                FroyoState::ThinPoolFailed => (0x200, 0),
                FroyoState::ThinFailed => (0x400, 0),
                FroyoState::Initializing => (0x2000, 0),
                FroyoState::Good(rs) => match rs {
                    FroyoRunningState::Degraded(x) => {
                        if x as usize >= REDUNDANCY {
                            (0, x as u32 | 0x100) // set "non-redundant" bit
                        } else {
                            (0, x as u32)
                        }
                    },
                    FroyoRunningState::Reshaping(_) => (0, 0x400),
                    FroyoRunningState::Good => (0, 0),
                },
            };

            if !self.is_reshapable() {
                r_status |= 0x200; // set "cannot reshape" bit
            }
            if self.throttled {
                r_status |= 0x800; // set "throttled" bit
            }

            try!(DbusContext::update_one(&dc.status_prop, status.into()));
            try!(DbusContext::update_one(&dc.running_status_prop, r_status.into()));

           let bdev_msg = DbusContext::get_block_devices_msgitem(&self.block_devs);
           try!(DbusContext::update_one(&dc.block_devices_prop, bdev_msg));
        }
        Ok(())
    }

    pub fn add_block_device(&mut self, path: &Path, force: bool) -> FroyoResult<()> {
        let bd = {
            match BlockDev::setup(path) {
                Ok(found_bd) => {
                    // Does the new blockdev's froyo id match us?
                    if found_bd.froyodev_id == self.id {
                        if self.block_devs.0.contains_key(&found_bd.id) {
                            if let BlockMember::Present(_) =
                                *self.block_devs.0.get(&found_bd.id).unwrap() {
                                    return Err(FroyoError::Froyo(InternalError(
                                        format!("Block member {} already present \
                                                 in froyodev {}",
                                                short_id(&found_bd.id), self.name).into())))
                                }

                            // TODO: treat as new if froyodev layout has changed
                            // Known but absent
                            let found_bd = Rc::new(RefCell::new(found_bd));
                            try!(self.raid_devs.add_existing_block_device(
                                &self.id, &found_bd));
                            found_bd
                        } else {
                            dbgp!("Block device {} mistakenly believes \
                                   it's part of froyodev {}, adding as new",
                                  path.display(), self.name);
                            let new_bd = Rc::new(RefCell::new(
                                try!(BlockDev::new(&self.id, path, force))));
                            try!(self.raid_devs.add_new_block_device(&self.id, &new_bd));
                            new_bd
                        }
                    } else {
                        // A blockdev from another froyodev, bad.
                        let buf = try!(found_bd.read_mdax());
                        let s = String::from_utf8_lossy(&buf).into_owned();
                        let froyo_save = try!(serde_json::from_str::<FroyoSave>(&s));
                        return Err(FroyoError::Froyo(InternalError(
                            format!("Block device {} is already part of froyodev \
                                     {}, id {}", path.display(), froyo_save.name,
                                    short_id(&froyo_save.id)).into())));
                    }
                },
                Err(_) => {
                    // setup() failed, so blockdev is not a current
                    // froyo member disk. Initialize and add it.
                    let bd = Rc::new(RefCell::new(
                        try!(BlockDev::new(&self.id, path, force))));
                    try!(self.raid_devs.add_new_block_device(&self.id, &bd));
                    bd
                }
            }
        };

        // Depending on the above, we either insert or update an
        // existing entry
        self.block_devs.0.insert(bd.borrow().id.clone(),
                                 BlockMember::Present(bd.clone()));

        Ok(())
    }

    // If removing blockdev would break a raid in the Froyodev, fail
    //
    // If blockdev is used by raids but they can continue degraded,
    // reload raids without LinearDevs from the blockdev
    //
    // Remove blockdev from self.block_devs, wipe superblock
    pub fn remove_block_device(&mut self, path: &Path, wipe_sb: bool) -> FroyoResult<()> {

        // Lookup the blockdev to remove.
        // NOTE: This blockdev is a copy of an item in self.block_devs
        // except its linear_devs is empty
        let mut blockdev = try!(BlockDev::setup(path));

        // Check this blockdev is in this froyodev
        if !self.block_devs.0.contains_key(&blockdev.id) {
            return Err(FroyoError::Froyo(InternalError(
                format!("{} is not a member of {}",
                        path.display(), self.name).into())))
        }

        // We could get affected lineardevs from looking at
        // self.block_devs[x].linear_devs but there's no backlink from
        // lineardevs to raids (and hard to add).
        // Instead, do it top-down.
        let raids_and_linears_using_dev = self.raid_devs.raids.iter_mut()
            .map(|(_, rd)| rd.borrow_mut())
            .filter_map(|rd| {
                let mut ld_index = None;
                for (count, rm) in rd.members.iter().enumerate() {
                    if let Some(pres) = rm.present() {
                        let parent_rc = pres.borrow().parent.upgrade().unwrap();
                        if parent_rc.borrow().dev == blockdev.dev {
                            // A raid may only have 1 lineardev on a
                            // given blockdev
                            ld_index = Some(count);
                            break
                        }
                    }
                }
                match ld_index {
                    None => None,
                    Some(ld_index) => Some((rd, ld_index)),
                }
            })
            .collect::<Vec<_>>();

        // Check status of each affected raid (if any)
        for &(ref raid, _) in &raids_and_linears_using_dev {
            let (status, action) = try!(raid.status());
            match status {
                RaidStatus::Good => {},
                RaidStatus::Degraded(x) if x < REDUNDANCY => {},
                _ => {
                    return Err(FroyoError::Froyo(InternalError(
                        format!("Cannot remove {}, a RAID would fail",
                                path.display()).into())))
                },
            }
            match action {
                RaidAction::Idle => {},
                x => {
                    return Err(FroyoError::Froyo(InternalError(
                        format!("Cannot remove {}, a RAID is in state {:?}",
                                path.display(), x).into())))
                }
            }
        }

        if !raids_and_linears_using_dev.is_empty() {
            let dm = try!(DM::new());

            // Reconfigure affected raids w/o the lineardev
            for (ref mut raid, ld_idx) in raids_and_linears_using_dev {
                // Take out Present value...
                let rm = raid.members[ld_idx].clone();
                let ld = rm.present().expect("should be here!!!");

                let new_rm = {
                    if wipe_sb {
                        RaidMember::Removed
                    } else {
                        let parent = ld.borrow().parent.upgrade().unwrap();
                        let parent_id = parent.borrow().id.clone();
                        RaidMember::Absent((parent_id, ld.borrow().to_save()))
                    }
                };
                // ..put in Removed or Absent value.
                raid.members[ld_idx] = new_rm;

                // TODO panic if reload fails???
                try!(raid.reload(&dm, None));
                try!(ld.borrow().teardown(&dm));
            }
        }

        if wipe_sb {
            self.block_devs.0.remove(&blockdev.id)
                .expect("blockdev should always still be here for us to remove");
            try!(blockdev.wipe_mda_header());
        } else {
            self.block_devs.0.insert(blockdev.id.clone(),
                                   BlockMember::Absent(blockdev.to_save()));
        }

        Ok(())
    }

    fn check_raidcopy(&mut self, mirror: MirrorDev)
                      -> FroyoResult<ReshapeState> {
        let dm = try!(DM::new());

        dbgp!("check_raidcopy");

        if try!(mirror.is_syncing(&dm)) {
            return Ok(ReshapeState::CopyingToRaid(mirror))
        }

        {
            // Syncing done! Switch to the fresh copy
            // Can't call save_state below with the rld still borrowed
            let mut rld = mirror.linear_dev.borrow_mut();

            try!(rld.dev.suspend(&dm));

            // splice the new location(s) into the RLD's list...
            // for raid->raid, there will only be one raidseg mirrored at
            // once.
            let idx = mirror.linear_dev_idxs[0];
            let mut rld_tail = rld.segments.split_off(idx);
            for &(ref dl, seg) in &mirror.dest.borrow().segments {
                rld.segments.push(
                    RaidSegment::new(seg.start, seg.length, RaidLayer::Raid(dl.raid())));
            }
            // remove the raidsegment we just synced...
            rld_tail.remove(0);
            // Put the rest back on the end
            rld.segments.extend(rld_tail);
        }

        try!(self.save_state());

        let rld = mirror.linear_dev.borrow();

        let table = RaidLinearDev::dm_table(&rld.segments);
        try!(rld.dev.table_load(&dm, &table));
        try!(rld.dev.unsuspend(&dm));

        try!(mirror.teardown(&dm));

        Ok(ReshapeState::Idle)
    }

    fn check_copy_to_scratch(&mut self, mirror: MirrorDev)
                             -> FroyoResult<ReshapeState> {
        let dm = try!(DM::new());

        dbgp!("check_copy_to_scratch");

        if try!(mirror.is_syncing(&dm)) {
            return Ok(ReshapeState::CopyingToScratch(mirror))
        }

        // Syncing done! Switch to just the scratch copy
        // then blow away the original degraded raid
        // Can't call save_state below with the rld still borrowed
        {
            let mut rld = mirror.linear_dev.borrow_mut();

            try!(rld.dev.suspend(&dm));

            // splice the new location(s) into the RLD's list...
            let mut offset = SectorOffset(0);
            let mut removed = Vec::new();
            for idx in &mirror.linear_dev_idxs {
                let mut rld_tail = rld.segments.split_off(*idx);
                let seg_len = rld_tail[0].length;

                // We have an order to our degraded raidsegs (their
                // indexes), and our linear mapping is in the same
                // order and equal to the sum of their sizes. The
                // mapping to scratch space we don't care about here,
                // we know the linear dev contains the mirrored
                // raidseg data and just need to offset into the dev
                // for each one.
                rld.segments.push(RaidSegment::new(
                    offset, seg_len, RaidLayer::Temp(mirror.dest.clone())));
                offset = offset + SectorOffset(*seg_len);
                // remove the raidsegment we just synced...
                removed.push(rld_tail.remove(0));
                // Put the rest back on the end
                rld.segments.extend(rld_tail);
            }

            // We're now running non-redundantly on scratch space. If we
            // crash, we need to be able to rebuild so we can continue
            // reshaping to regain redundancy.
            self.raid_devs.temp_dev = Some(mirror.dest.clone());

            let removed_rd = removed.iter().next().unwrap().parent.raid();
            if removed.iter().any(|rs| rs.parent.raid() != removed_rd) {
                panic!("all removed raidsegs are not on the same raiddev!");
            }
        }

        try!(self.save_state());

        let rld = mirror.linear_dev.borrow();

        let table = RaidLinearDev::dm_table(&rld.segments);
        try!(rld.dev.table_load(&dm, &table));
        try!(rld.dev.unsuspend(&dm));

        try!(mirror.teardown(&dm));

        // We're now running non-redundantly off scratch. The state
        // machine should next look to zap and rebuild the now-unused
        // degraded raid. Once that syncs, the state machine will
        // attempt to move stuff on scratch back onto the new Raiddev.
        Ok(ReshapeState::Idle)
    }

    fn check_copy_from_scratch(&mut self, mirror: MirrorDev)
                               -> FroyoResult<ReshapeState> {
        let dm = try!(DM::new());

        dbgp!("check_copy_from_scratch");

        if try!(mirror.is_syncing(&dm)) {
            return Ok(ReshapeState::CopyingFromScratch(mirror))
        }

        {
            // Syncing done! Switch to the redundant copy
            // Can't call save_state below with the rld still borrowed
            let mut rld = mirror.linear_dev.borrow_mut();

            try!(rld.dev.suspend(&dm));

            let mut dest = mirror.dest.borrow().segments.clone();

            // So we can pop head
            dest.reverse();

            // Replace each RaidSegment on TempDevs with 1 or more
            // RaidSegments on RaidDevs. This is unpleasant because seg
            // boundaries don't line up, although we know the overall
            // lengths will match.
            let (mut tl, mut dest_seg) = dest.pop().unwrap();
            for idx in &mirror.linear_dev_idxs {
                let mut rld_tail = rld.segments.split_off(*idx);
                let mut len = rld_tail[0].length;
                rld_tail.remove(0);

                while len > Sectors(0) {
                    if len >= dest_seg.length {
                        len = len - dest_seg.length;
                        rld.segments.push(RaidSegment::new(
                            dest_seg.start,
                            dest_seg.length,
                            RaidLayer::Raid(tl.raid().clone())));
                        let temp = dest.pop().unwrap();
                        tl = temp.0;
                        dest_seg = temp.1;
                    } else {
                        // split dest_seg into two, pushing the first and
                        // leaving the second as dest_seg.
                        let new_dest_seg = LinearSegment::new(
                            dest_seg.start + SectorOffset(*len),
                            dest_seg.length - len);
                        rld.segments.push(RaidSegment::new(
                            dest_seg.start,
                            len,
                            RaidLayer::Raid(tl.raid().clone())));
                        // tl stays the same
                        dest_seg = new_dest_seg;
                        len = Sectors(0);
                    }
                }

                // Put the rest back on the end
                rld.segments.extend(rld_tail);
            }

            // We just copied back onto redundant space. We no longer
            // need to track a temp dev in our saved metadata.
            self.raid_devs.temp_dev = None;
        }

        try!(self.save_state());

        let rld = mirror.linear_dev.borrow();

        let table = RaidLinearDev::dm_table(&rld.segments);
        try!(rld.dev.table_load(&dm, &table));
        try!(rld.dev.unsuspend(&dm));

        try!(mirror.teardown(&dm));

        Ok(ReshapeState::Idle)
    }

    fn check_resync(&self) -> FroyoResult<ReshapeState> {
        dbgp!("check_resync");
        if self.raid_devs.are_idle() {
            Ok(ReshapeState::Idle)
        } else {
            Ok(ReshapeState::SyncingRaids)
        }
    }

    //
    // NOTE: It is UNSAFE to allocate space on blockdevs
    // (e.g. creating a new raid) or raiddevs (e.g.extending a
    // RaidLinearDev) while a reshape is ongoing. Temporary
    // allocations are not tracked and would likely double-allocate.
    //
    fn reshape_state_machine(&mut self, state: ReshapeState) -> FroyoResult<ReshapeState> {
        match state {
            ReshapeState::Off => Ok(ReshapeState::Off),
            ReshapeState::CopyingToRaid(mir) => self.check_raidcopy(mir),
            ReshapeState::CopyingToScratch(mir) => self.check_copy_to_scratch(mir),
            ReshapeState::CopyingFromScratch(mir) => self.check_copy_from_scratch(mir),
            ReshapeState::SyncingRaids => self.check_resync(),
            ReshapeState::Idle => {

                let r = try!(self.recreate_empty_degraded_raids());
                if r.is_busy() {
                    dbgp!("recreating empty degraded raids");
                    return Ok(r)
                }

                let md = self.thin_pool_dev.meta_dev.clone();
                let r = try!(self.start_copy_from_scratch(md));
                if r.is_busy() {
                    dbgp!("reshaping meta from scratch");
                    return Ok(r)
                }

                let dd = self.thin_pool_dev.data_dev.clone();
                let r = try!(self.start_copy_from_scratch(dd));
                if r.is_busy() {
                    dbgp!("reshaping data from scratch");
                    return Ok(r)
                }

                let r = try!(self.start_copy_to_safe_raid(&self.thin_pool_dev.meta_dev));
                if r.is_busy() {
                    dbgp!("reshaping meta to safe raid");
                    return Ok(r)
                }

                let r = try!(self.start_copy_to_safe_raid(&self.thin_pool_dev.data_dev));
                if r.is_busy() {
                    dbgp!("reshaping data to safe raid");
                    return Ok(r)
                }

                let r = try!(self.start_copy_to_scratch(&self.thin_pool_dev.meta_dev));
                if r.is_busy() {
                    dbgp!("reshaping meta to scratch");
                    return Ok(r)
                }

                let r = try!(self.start_copy_to_scratch(&self.thin_pool_dev.data_dev));
                if r.is_busy() {
                    dbgp!("reshaping data to scratch");
                    return Ok(r)
                }

                dbgp!("reshape stopping");
                Ok(ReshapeState::Off)
            }
        }
    }

    pub fn reshape(&mut self) -> FroyoResult<()> {
        // Summary:
        // phase 1: get redundant
        // phase 2: use all space efficiently
        //
        // thinpool extend needed while reshape? cancel reshape. (how?)

        if !self.is_reshapable() {
            dbgp!("cannot initiate a reshape!");
            return Err(FroyoError::Froyo(InternalError("Cannot reshape".into())))
        }

        dbgp!("starting reshaping!");
        self.last_state = match try!(self.reshape_state_machine(ReshapeState::Idle)) {
            ReshapeState::Off => FroyoState::Good(FroyoRunningState::Good),
            x => FroyoState::Good(FroyoRunningState::Reshaping(x)),
        };

        Ok(())
    }

    pub fn check_state(&mut self) -> FroyoResult<()> {

        if let FroyoState::Initializing = self.last_state {
            let cur_state = try!(self.status());

            if let FroyoState::Good(_) = cur_state {
                try!(self.initial_resync_complete());
            }

            return Ok(())
        }

        // TODO: simplify this once Rust has non-lexical closures
        // (can't set self.last_state within a match on self.last_state)
        let r_state = match self.last_state {
            FroyoState::Good(FroyoRunningState::Reshaping(ref state)) => {
                Some(state.clone())
            }
            _ => return self.handle_thinpool_usage(),
        };

        if let Some(state) = r_state {
            self.last_state = match try!(self.reshape_state_machine(state)) {
                ReshapeState::Off => FroyoState::Good(FroyoRunningState::Good),
                x => FroyoState::Good(FroyoRunningState::Reshaping(x)),
            };
        };

        Ok(())
    }

    // We may be reshaping either to reestablish redundancy on a
    // smaller number of blockdevs (shrink), or to take advantage of
    // more blockdevs (expand).
    // To be sure we can shrink, we need sufficient scratch space and
    // we need to check the reshaped raids will have capacity for how
    // much data we have.
    // Even to expand we need scratch space enough to make a copy of the
    // most-used raiddev's data.
    pub fn is_reshapable(&self) -> bool {
        // Just one disk, no way we can re-establish redundancy
        if self.block_devs.0.iter()
            .filter_map(|(_, bd)| bd.present())
            .count() < 2 {
                dbgp!("can't reshape, only 1 dev");
                return false
            }

        if !self.raid_devs.are_idle() {
            dbgp!("can't reshape, not all idle");
            return false
        }

        // scratch space must be greater than largest used space in
        // any degraded raid (so we can make a temp copy)
        if self.raid_devs.max_used_raid_sectors() > self.block_devs.unused_space() {
            dbgp!("can't reshape, not enough free scratch space");
            return false
        }

        // After reshape, how much redundant space will we have?
        let reshaped_tot_size = self.raid_devs.raids.iter()
            .map(|(_, rd)| {
                let rd = rd.borrow();
                let per_member_data_size = rd.per_member_size().unwrap().1;
                let members_present = rd.members.iter()
                    .filter_map(|rm| rm.present())
                    .count();
                let sz = (members_present - REDUNDANCY) * *per_member_data_size as usize;
                Sectors(sz as u64)
            })
            .sum::<Sectors>();

        if self.thin_pool_dev.used_sectors() > reshaped_tot_size {
            dbgp!("can't reshape, too much data for reshaped froyodev");
            return false
        }

        if let FroyoState::Good(FroyoRunningState::Reshaping(_)) = self.last_state {
            dbgp!("can't reshape, already reshaping");
            return false
        }

        dbgp!("can reshape");
        true
    }

    pub fn needs_reshape(&self) -> FroyoResult<bool> {
        // is any raid degraded or failed?
        for rd in self.raid_devs.raids.values() {
            let (r_status, _) = try!(rd.borrow().status());
            match r_status {
                RaidStatus::Failed => panic!("raid failed, cannot reshape"),
                RaidStatus::Degraded(_) => return Ok(true),
                RaidStatus::Good => {},
            }
        }

        // is there an empty disk?
        // TODO: check it's large enough we'd use it in a raid
        for bd in self.block_devs.0.values() {
            if let BlockMember::Present(ref bd) = *bd {
                if bd.borrow().linear_devs.is_empty() {
                    return Ok(true)
                }
            }
        }

        Ok(false)
    }

    fn recreate_empty_degraded_raids(&mut self) -> FroyoResult<ReshapeState> {
        let dm = try!(DM::new());

        let mut removed = Vec::new();
        for (id, rd) in &mut self.raid_devs.raids {
            let mut rd = rd.borrow_mut();
            if rd.is_empty() && !rd.is_safe() {
                try!(rd.destroy(&dm));
                removed.push(id.clone());
            }
        }

        if !removed.is_empty() {
            for id in removed {
                self.raid_devs.raids.remove(&id);
            }
            try!(self.save_state());
        }

        if try!(self.raid_devs.create_redundant_zones(&dm, &self.id, &self.block_devs)) {
            dbgp!("created new raids, syncing");
            try!(self.save_state());
            Ok(ReshapeState::SyncingRaids)
        } else {
            dbgp!("no new raids created");
            Ok(ReshapeState::Idle)
        }
    }

    // If there's a RaidSegment on an unsafe raid, create a raid1 on
    // top of it to copy it to a safe raid while remaining online.
    fn start_copy_to_safe_raid(&self, src: &Rc<RefCell<RaidLinearDev>>)
                               -> FroyoResult<ReshapeState> {
        // Find first segment on a degraded raid, or exit
        let b_src = src.borrow();
        let (idx, raid_seg) = match b_src.segments.iter().enumerate()
            .find(|&(_, rs)| {
                let rd = rs.parent.raid();
                let b_rd = rd.borrow();
                !b_rd.is_safe()
            }) {
                None => return Ok(ReshapeState::Idle),
                Some(rs) => rs,
            };

        // Get space on good raids
        let new_space = match self.raid_devs.alloc_raid_segments(raid_seg.length) {
            // Dont panic yet, maybe we just need to use scratch space
            None => return Ok(ReshapeState::Idle),
            Some(rsegs) => {
                rsegs.iter()
                    .map(|rs| (
                        TempLayer::Raid(rs.parent.raid().clone()),
                        LinearSegment::new(rs.start, rs.length)))
                    .collect::<Vec<_>>()
            }
        };

        let dm = try!(DM::new());

        // create the source linear mapping target
        let src_dev = try!(TempDev::new(
            &dm, &self.id, &[(
                TempLayer::Raid(raid_seg.parent.raid().clone()),
                LinearSegment::new(raid_seg.start, raid_seg.length))]));

        // create the destination linear mapping target
        let dest_dev = try!(TempDev::new(&dm, &self.id, &new_space));

        try!(b_src.dev.suspend(&dm));
        let mirror_dev = try!(MirrorDev::new(
            &dm, &self.id, Rc::new(RefCell::new(src_dev)),
            Rc::new(RefCell::new(dest_dev)), raid_seg.length, src.clone(), &[idx]));

        // Get the dm table but switch out for our new mirror
        let mut table = RaidLinearDev::dm_table(&b_src.segments);
        table[idx] = (table[idx].0, table[idx].1, table[idx].2.clone(),
                      format!("{} 0", mirror_dev.mirror.dstr()));

        try!(b_src.dev.table_load(&dm, &table));
        try!(b_src.dev.unsuspend(&dm));

        Ok(ReshapeState::CopyingToRaid(mirror_dev))
    }

    // Start a two-step process of copying to non-redundant scratch
    // space, remaking the existing raiddev, and then copying back,
    // since there isn't enough room in other existing raids.
    fn start_copy_to_scratch(&self, src: &Rc<RefCell<RaidLinearDev>>)
                             -> FroyoResult<ReshapeState> {
        let b_src = src.borrow();

        // We need to copy all raidsegs on a degraded raid to scratch, because
        // we want it to be empty so we can destroy/remake it. fml...
        let mut v: BTreeMap<String, Vec<(usize, &RaidSegment)>> = BTreeMap::new();
        for (idx, rs) in b_src.segments.iter().enumerate()
            .filter(|&(_, rs)| {
                let rd = rs.parent.raid();
                let b_rd = rd.borrow();
                !b_rd.is_safe()
            })
        {
            let rd = rs.parent.raid();
            let brd = rd.borrow();
            v.entry(brd.id.to_owned())
                .or_insert_with(Vec::new)
                .push((idx, rs));
        }

        if v.is_empty() {
            return Ok(ReshapeState::Idle);
        }

        // We're doing an N:M mapping of raidsegs to scratch space.
        // The src & dest linear maps don't care where the original
        // raidseg boundaries were -- we just need to keep them in
        // order, keep the list of indexes into the raidlineardev's
        // segments list in order, and then we'll use rld's segments
        // list (which has lengths) to know the lengths of each
        // raidseg.

        let raid_segs = v.values().next().unwrap();
        let raid_idxs = raid_segs.iter()
            .map(|&(idx, _)| idx)
            .collect::<Vec<_>>();
        let raid_segs_ls = raid_segs.iter()
            .map(|&(_, rs)|
                 (TempLayer::Raid(rs.parent.raid().clone()),
                                 LinearSegment::new(rs.start, rs.length)))
            .collect::<Vec<_>>();
        let spc_needed = raid_segs.iter()
            .map(|&(_, ls)| ls.length)
            .sum::<Sectors>();

        let scratch_areas = try!(
            self.block_devs.get_linear_segments(spc_needed)
                .ok_or(FroyoError::Froyo(InternalError(
                    format!("No scratch space for raidseg {}",
                            *spc_needed).into()))));
        let scratch_areas = scratch_areas.into_iter()
            .map(|(bd, ls)| (TempLayer::Block(bd), ls))
            .collect::<Vec<_>>();

        let dm = try!(DM::new());

        // create the source linear mapping target
        let src_dev = try!(TempDev::new(&dm, &self.id, &*raid_segs_ls));

        // create the destination linear mapping target
        let dest_dev = try!(TempDev::new(&dm, &self.id, &*scratch_areas));

        try!(b_src.dev.suspend(&dm));
        let mirror_dev = try!(MirrorDev::new(
            &dm, &self.id, Rc::new(RefCell::new(src_dev)),
            Rc::new(RefCell::new(dest_dev)), spc_needed, src.clone(), &*raid_idxs));

        // Get the dm table but switch out for our new mirror
        let mut table = RaidLinearDev::dm_table(&b_src.segments);
        let mut offset = SectorOffset(0);
        for &(idx, rs) in raid_segs {
            table[idx] = (table[idx].0, table[idx].1, table[idx].2.clone(),
                          format!("{} {}", mirror_dev.mirror.dstr(), *offset));
            offset = offset + SectorOffset(*rs.length);
        }

        try!(b_src.dev.table_load(&dm, &table));
        try!(b_src.dev.unsuspend(&dm));

        Ok(ReshapeState::CopyingToScratch(mirror_dev))
    }

    // Start copying back from scratch space to redundant space
    fn start_copy_from_scratch(&mut self, dest: Rc<RefCell<RaidLinearDev>>)
                               -> FroyoResult<ReshapeState> {
        if !dest.borrow().segments.iter().any(|seg| seg.parent.on_temp()) {
            return Ok(ReshapeState::Idle)
        }

        let src_dev = match self.raid_devs.temp_dev.take() {
            Some(td) => td,
            None => return Ok(ReshapeState::Idle),
        };

        let len = src_dev.borrow().length();
        let raid_segs = try!(
            self.raid_devs.alloc_raid_segments(len)
                .ok_or_else(||
                            io::Error::new(io::ErrorKind::InvalidInput,
                                           "no space to copy back from scratch")));
        let idxs = dest.borrow().segments.iter().enumerate()
            .filter(|&(_, rs)| rs.parent.on_temp())
            .map(|(idx, _)| idx)
            .collect::<Vec<_>>();

        let dm = try!(DM::new());

        // Make a linear TempDev that covers our raided space
        let tmp_segments = raid_segs.iter()
            .map(|rs| (TempLayer::Raid(rs.parent.raid().clone()),
                       LinearSegment::new(rs.start, rs.length)))
            .collect::<Vec<_>>();
        let dest_dev = try!(TempDev::new(&dm, &self.id, &tmp_segments));

        try!(dest.borrow().dev.suspend(&dm));
        let mirror_dev = try!(MirrorDev::new(
            &dm, &self.id, src_dev, Rc::new(RefCell::new(dest_dev)),
            len, dest.clone(), &idxs));

        let b_dest = dest.borrow();

        // Get the dm table but switch out for our new mirror
        let mut table = RaidLinearDev::dm_table(&b_dest.segments);
        let mut offset = SectorOffset(0);
        for (idx, rs) in b_dest.segments.iter().enumerate()
            .filter(|&(_, rs)| rs.parent.on_temp())
        {
            table[idx] = (table[idx].0, table[idx].1, table[idx].2.clone(),
                          format!("{} {}", mirror_dev.mirror.dstr(), *offset));
            offset = offset + SectorOffset(*rs.length);
        }

        try!(b_dest.dev.table_load(&dm, &table));
        try!(b_dest.dev.unsuspend(&dm));

        Ok(ReshapeState::CopyingFromScratch(mirror_dev))
    }

    pub fn handle_thinpool_usage(&mut self) -> FroyoResult<()> {

        if self.last_state.is_reshaping() {
            // TODO: if we're getting low on space, start using
            // dm-delay so we don't run out of space while we're
            // reshaping
            return Ok(())
        }

        match try!(self.thin_pool_dev.status()) {
            ThinPoolStatus::Fail => panic!("thinpool is failed!"),
            ThinPoolStatus::Good((status, usage)) => {
                match status {
                    ThinPoolWorkingStatus::Good => {
                        let remaining_data = usage.total_data - usage.used_data;
                        if remaining_data < self.thin_pool_dev.low_water_blocks {
                            try!(self.extend_thinpool_data_dev(TPOOL_EXTEND_SECTORS));
                            try!(self.save_state());
                        }

                        let remaining_meta = usage.total_meta - usage.used_meta;
                        // TODO meta low-water # should probably be an independent value
                        if remaining_meta < *self.thin_pool_dev.low_water_blocks {
                            // Double it
                            let meta_secs = self.thin_pool_dev.meta_dev.borrow().length();
                            try!(self.extend_thinpool_meta_dev(meta_secs));
                            try!(self.save_state());
                        }
                    }
                    _ => panic!(format!("bad thin check_status: {:?}", status))
                }
            }

        };

        Ok(())

        // TODO check & extend thin devs
        // for thin in &self.thin_devs {
        //     match try!(thin.status()) {
        //         ThinStatus::Fail => dbgp!("thin #{} failed", thin.thin_number),
        //         ThinStatus::Good(sectors) => dbgp!("thin #{} using {} sectors",
        //                                            thin.thin_number,
        //                                            *sectors),
        //     }
        // }
    }

    #[allow(cyclomatic_complexity)]
    pub fn dump_status(&self) -> FroyoResult<()> {
        dbgp!("Froyo name: {}", self.name);

        dbgp!("Block devs: {}", self.block_devs.0.len());
        for bm in self.block_devs.0.values() {
            match *bm {
                BlockMember::Present(ref bd) => {
                    let bd = bd.borrow();
                    dbgp!("  dev {} largest avail {}",
                          bd.path.display(),
                          bd.largest_avail_area().map(|(_, x)| *x).unwrap_or(0u64));
                },
                BlockMember::Absent(ref bds) => {
                    dbgp!("  dev {} absent", bds.path.display());
                },
            }
        }

        for (raid_count, raid) in self.raid_devs.raids.values().enumerate() {
            let raid = raid.borrow();
            let (status, action) = try!(raid.status());
            dbgp!("Raid dev #{} status {:?} action {:?}", raid_count, status, action);
            for (leg_count, rm) in raid.members.iter().enumerate() {
                match *rm {
                    RaidMember::Present(ref ld) => {
                        dbgp!("  #{} size {}", leg_count, *ld.borrow().data_length());
                    },
                    RaidMember::Absent(_) => dbgp!("  #{} absent", leg_count),
                    RaidMember::Removed => dbgp!("  #{} removed", leg_count),
                }
            }
        }

        let tp_status = try!(self.thin_pool_dev.status());
        let txt_status = match tp_status {
            ThinPoolStatus::Fail => "Failed".to_owned(),
            ThinPoolStatus::Good((status, usage)) => format!(
                "{:?}, {} of {} data, {} of {} meta blocks used",
                status, *usage.used_data,
                *usage.total_data,
                usage.used_meta,
                usage.total_meta),
        };
        dbgp!("thin pool dev status: {}", txt_status);
        // TODO: Fix this Rule of Demeter violation
        dbgp!("thin pool meta on {} raids",
              self.thin_pool_dev.meta_dev.borrow().parents().len());
        dbgp!("thin pool data on {} raids",
              self.thin_pool_dev.data_dev.borrow().parents().len());

        for thin in &self.thin_devs {
            match try!(thin.status()) {
                ThinStatus::Fail => dbgp!("thin #{} failed", thin.thin_number),
                ThinStatus::Good(sectors) => dbgp!(
                    "thin #{} using {}", thin.thin_number,
                    ByteSize::b((*sectors * SECTOR_SIZE) as usize).to_string(true)),
            }
        }

        dbgp!("");

        Ok(())
    }
}
