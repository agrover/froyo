// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Borrow;
use std::path::Path;
use std::cmp::{Ordering, min, max};
use std::io;
use std::io::ErrorKind;
use std::error::Error;

use uuid::Uuid;
use devicemapper::DM;
use serde_json;
use time;
use bytesize::ByteSize;

use blockdev::{BlockDev, BlockDevSave, BlockMember};
use blockdev::{LinearDev, LinearSegment};
use raid::{RaidDev, RaidDevSave, RaidSegment, RaidLinearDev, RaidStatus,
           RaidAction, RaidMember};
use thin::{ThinPoolDev, ThinPoolDevSave, ThinPoolStatus, ThinPoolWorkingStatus};
use thin::{ThinDev, ThinDevSave, ThinStatus};
use types::{Sectors, SectorOffset, DataBlocks, FroyoError, FroyoResult, InternalError};
use dbus_api::DbusContext;
use util::align_to;
use consts::*;


#[derive(Debug, Clone, Serialize, Deserialize)]
struct FroyoSave {
    name: String,
    id: String,
    block_devs: BTreeMap<String, BlockDevSave>,
    raid_devs: BTreeMap<String, RaidDevSave>,
    thin_pool_dev: ThinPoolDevSave,
    thin_devs: Vec<ThinDevSave>,
}

#[derive(Debug, Clone)]
pub struct Froyo<'a> {
    pub id: String,
    pub name: String,
    pub block_devs: BTreeMap<String, BlockMember>,
    raid_devs: BTreeMap<String, Rc<RefCell<RaidDev>>>,
    thin_pool_dev: ThinPoolDev,
    thin_devs: Vec<ThinDev>,
    throttled: bool,
    reshaping: bool,
    pub dbus_context: Option<DbusContext<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub enum FroyoStatus {
    Good(FroyoRunningStatus),
    RaidFailed,
    ThinPoolFailed,
    ThinFailed,
}

#[derive(Debug, Clone, Copy)]
pub enum FroyoRunningStatus {
    Good,
    Degraded(u8),
    Throttled,
}

impl<'a> Froyo<'a> {
    pub fn new<T>(name: &str, paths: &[T], force: bool)
                     -> FroyoResult<Froyo<'a>>
        where T: Borrow<Path>
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
        let mut block_devs = BTreeMap::new();
        for path in paths {
            let bd = try!(BlockDev::new(&froyo_id, path.borrow(), force));
            block_devs.insert(bd.id.clone(),
                              BlockMember::Present(Rc::new(RefCell::new(bd))));
        }

        let dm = try!(DM::new());

        let mut raid_devs = BTreeMap::new();
        while let Some(rd) = try!(
            Froyo::create_redundant_zone(&dm, &froyo_id, &block_devs)) {
            raid_devs.insert(rd.id.clone(), Rc::new(RefCell::new(rd)));
        }

        let meta_size = TPOOL_INITIAL_META_SECTORS;
        let data_size = TPOOL_INITIAL_DATA_SECTORS;

        let meta_raid_segments = try!(Self::get_raid_segments(
            meta_size, &raid_devs).ok_or_else(||
            io::Error::new(io::ErrorKind::InvalidInput,
                           "no space for thinpool meta")));
        let data_raid_segments = try!(Self::get_raid_segments(
            data_size, &raid_devs).ok_or_else(||
            io::Error::new(io::ErrorKind::InvalidInput,
                           "no space for thinpool data")));
        let thin_pool_dev = try!(ThinPoolDev::new(
            &dm, &froyo_id, meta_raid_segments, data_raid_segments));

        let mut thin_devs = Vec::new();
        // Create an initial 1TB thin dev
        thin_devs.push(try!(ThinDev::new(
            &dm,
            &froyo_id,
            name, // 1st thindev name same as froyodev name
            0,
            THIN_INITIAL_SECTORS,
            &thin_pool_dev)));

        Ok(Froyo {
            name: name.to_owned(),
            id: froyo_id,
            block_devs: block_devs,
            raid_devs: raid_devs,
            thin_pool_dev: thin_pool_dev,
            thin_devs: thin_devs,
            throttled: false,
            reshaping: false,
            dbus_context: None,
        })
    }

    fn to_save(&self) -> FroyoSave {
        FroyoSave {
            name: self.name.to_owned(),
            id: self.id.to_owned(),
            block_devs: self.block_devs.iter()
                .map(|(id, bd)| {
                    match *bd {
                        BlockMember::Present(ref bd) =>
                            (id.clone(), RefCell::borrow(&bd).to_save()),
                        BlockMember::Absent(ref sbd) =>
                            (id.clone(), sbd.clone()),
                    }
                })
                .collect(),
            raid_devs: self.raid_devs.iter()
                .map(|(id, rd)| (id.clone(), RefCell::borrow(rd).to_save()))
                .collect(),
            thin_pool_dev: self.thin_pool_dev.to_save(),
            thin_devs: self.thin_devs.iter()
                .map(|x| x.to_save())
                .collect(),
        }
    }

    pub fn destroy(mut self) -> FroyoResult<()> {
        try!(self.teardown());

        for bd in self.block_devs.values() {
            if let BlockMember::Present(ref bd) = *bd {
                if let Err(e) = RefCell::borrow_mut(&*bd).wipe_mda_header() {
                    dbgp!("Error when wiping header: {}", e.description());
                }
            }
        }

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
        -> FroyoResult<BTreeMap<String, BlockMember>> {
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

        Ok(block_devs)
    }

    fn setup_raiddev(
        dm: &DM,
        froyo_id: &str,
        raid_id: &str,
        raid_save: &RaidDevSave,
        block_devs: &BTreeMap<String, BlockMember>)
        -> FroyoResult<RaidDev> {
        let mut linear_devs = Vec::new();

        // Loop through saved struct and setup legs if present in both
        // the blockdev list and the raid members list
        for count in 0..raid_save.member_count {
            match raid_save.members.get(&count.to_string()) {
                Some(sld) => {
                    match block_devs.get(&sld.parent) {
                        Some(bm) => {
                            match *bm {
                                BlockMember::Present(ref bd) => {
                                    let ld = Rc::new(RefCell::new(try!(LinearDev::setup(
                                        &dm,
                                        &format!("{}-{}-{}", froyo_id, raid_id, count),
                                        &bd,
                                        &sld.meta_segments,
                                        &sld.data_segments))));
                                    bd.borrow_mut().linear_devs.push(ld.clone());
                                    linear_devs.push(RaidMember::Present(ld));
                                },
                                BlockMember::Absent(_) => {
                                    dbgp!("Expected device absent from raid {}", raid_id);
                                    linear_devs.push(
                                        RaidMember::Absent((sld.parent.clone(),
                                                            sld.clone())));
                                },
                            }
                        },
                        None => {
                            return Err(FroyoError::Io(io::Error::new(
                                io::ErrorKind::InvalidInput,
                                format!("Invalid metadata, raiddev {} references \
                                         blockdev {} that is not found in \
                                         blockdev list",
                                        raid_id, sld.parent))))
                        },
                    }
                },
                None => {
                    dbgp!("Raid {} member not present", raid_id);
                    linear_devs.push(RaidMember::Removed);
                },
            }
        }

        RaidDev::setup(
            &dm,
            &froyo_id,
            raid_id.to_owned(),
            linear_devs,
            raid_save.stripe_sectors,
            raid_save.region_sectors)
    }

    fn setup_raiddevs(
        dm: &DM,
        froyo_save: &FroyoSave,
        block_devs: &BTreeMap<String, BlockMember>)
        -> FroyoResult<BTreeMap<String, Rc<RefCell<RaidDev>>>> {
        let mut raid_devs = BTreeMap::new();
        for (id, srd) in &froyo_save.raid_devs {
            let rd = Rc::new(RefCell::new(try!(Self::setup_raiddev(
                &dm,
                &froyo_save.id,
                id,
                srd,
                block_devs))));
            let id = RefCell::borrow(&rd).id.clone();

            raid_devs.insert(id, rd);
        }
        Ok(raid_devs)
    }

    fn setup_thinpool(
        dm: &DM,
        froyo_save: &FroyoSave,
        raid_devs: &BTreeMap<String, Rc<RefCell<RaidDev>>>)
        -> FroyoResult<ThinPoolDev> {
        let tpd = &froyo_save.thin_pool_dev;

        let meta_name = format!("thin-meta-{}", froyo_save.id);
        let mut raid_segments = Vec::new();
        for seg in &tpd.meta_dev.segments {
            let parent = try!(raid_devs.get(&seg.parent).ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput,
                               "Could not find meta's parent")}));
            raid_segments.push(
                RaidSegment::new(seg.start, seg.length, parent));
        }

        let meta_raid_dev = try!(RaidLinearDev::setup(
            &dm,
            &meta_name,
            &tpd.meta_dev.id,
            raid_segments));

        let data_name = format!("thin-data-{}", froyo_save.id);
        let mut raid_segments = Vec::new();
        for seg in &tpd.data_dev.segments {
            let parent = try!(raid_devs.get(&seg.parent).ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput,
                               "Could not find data's parent")}));
            raid_segments.push(
                RaidSegment::new(seg.start, seg.length, parent));
        }

        let data_raid_dev = try!(RaidLinearDev::setup(
            &dm,
            &data_name,
            &tpd.data_dev.id,
            raid_segments));

        ThinPoolDev::setup(
            &dm,
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

        let raid_devs = try!(Froyo::setup_raiddevs(&dm, &froyo_save, &block_devs));

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

        Ok(Froyo {
            name: froyo_save.name.to_owned(),
            id: froyo_id.to_owned(),
            block_devs: block_devs,
            raid_devs: raid_devs,
            thin_pool_dev: thin_pool_dev,
            thin_devs: thin_devs,
            throttled: false,
            reshaping: false,
            dbus_context: None,
        })
    }

    pub fn teardown(&mut self) -> FroyoResult<()> {
        let dm = try!(DM::new());

        for thin in &mut self.thin_devs {
            try!(thin.teardown(&dm))
        }

        try!(self.thin_pool_dev.teardown(&dm));

        for raid in &mut self.raid_devs.values() {
            try!(RefCell::borrow_mut(raid).teardown(&dm))
        }

        Ok(())
    }

    // Try to make an as-large-as-possible redundant device from the
    // given block devices.
    fn create_redundant_zone(
        dm: &DM,
        name: &str,
        block_devs: &BTreeMap<String, BlockMember>)
        -> FroyoResult<Option<RaidDev>> {

        // get common data area size, allowing for Froyo data at start and end
        let mut bd_areas: Vec<_> = block_devs.values()
            .filter_map(|bd| {
                if let BlockMember::Present(ref bd) = *bd {
                    Some(bd)
                } else {
                    None
                }})
            .filter_map(|bd| {
                match RefCell::borrow(&*bd).largest_avail_area() {
                    Some(x) => Some((bd.clone(), x)),
                    None => None,
                }
            })
            .filter(|&(_, (_, len))| len >= MIN_DATA_ZONE_SECTORS)
            .collect();

        // Not enough devs with room for a raid device
        if bd_areas.len() < 2 {
            return Ok(None)
        }

        let common_avail_sectors = bd_areas.iter()
            .map(|&(_, (_, len))| len)
            .min()
            .unwrap();

        // Absolute limit on each RAID size.
        let common_avail_sectors = min(common_avail_sectors, MAX_DATA_ZONE_SECTORS);

        // Also limit size in order to try to create a certain base
        // number of RAIDs, for reshape shenanigans.
        // Use size of 2nd largest bdev, which is guaranteed to be
        // used fully by raids, unlike the largest.
        let second_largest_bdev = {
            let mut sizes = block_devs.values()
                .filter_map(|bm| bm.present())
                .map(|bd| RefCell::borrow(&*bd).sectors)
                .collect::<Vec<_>>();
            sizes.sort();
            sizes.pop();
            sizes.pop().unwrap()
        };
        let clamped_size = max(
            second_largest_bdev / Sectors::new(IDEAL_RAID_COUNT as u64),
            MIN_DATA_ZONE_SECTORS);
        let common_avail_sectors = min(common_avail_sectors, clamped_size);

        let (region_count, region_sectors) = {
            let mut region_sectors = DEFAULT_REGION_SECTORS;
            while *common_avail_sectors / *region_sectors > MAX_REGIONS {
                region_sectors = Sectors::new(*region_sectors * 2);
            }

            let partial_region = if common_avail_sectors % region_sectors == Sectors::new(0) {
                Sectors::new(0)
            } else {
                Sectors::new(1)
            };

            (common_avail_sectors / region_sectors + partial_region, region_sectors)
        };

        // each region needs 1 bit in the write intent bitmap
        let mdata_sectors = Sectors::new(align_to(8192 + (*region_count / 8) , SECTOR_SIZE)
                                         .next_power_of_two()
                                         / SECTOR_SIZE);
        // data size must be multiple of stripe size
        let data_sectors = (common_avail_sectors - mdata_sectors)
            & Sectors::new(!(*STRIPE_SECTORS-1));

        let raid_uuid = Uuid::new_v4().to_simple_string();

        let mut linear_devs = Vec::new();
        for (num, &mut(ref mut bd, (sector_start, _))) in bd_areas.iter_mut().enumerate() {
            let mdata_sector_start = sector_start;
            let data_sector_start = SectorOffset::new(*mdata_sector_start + *mdata_sectors);

            let linear = Rc::new(RefCell::new(try!(LinearDev::new(
                &dm,
                &format!("{}-{}-{}", name, raid_uuid, num),
                bd,
                &[LinearSegment {
                    start: mdata_sector_start,
                    length: mdata_sectors,
                }],
                &[LinearSegment {
                    start: data_sector_start,
                    length: data_sectors,
                }]))));

            bd.borrow_mut().linear_devs.push(linear.clone());
            linear_devs.push(RaidMember::Present(linear));
        }

        let raid = try!(RaidDev::setup(
            &dm,
            &name,
            raid_uuid,
            linear_devs,
            STRIPE_SECTORS,
            region_sectors));

        Ok(Some(raid))
    }

    pub fn save_state(&self) -> FroyoResult<()> {
        let metadata = try!(self.to_metadata());
        let current_time = time::now().to_timespec();

        for bd in self.block_devs.values() {
            if let BlockMember::Present(ref bd) = *bd {
                try!(bd.borrow_mut().save_state(&current_time, metadata.as_bytes()))
            }
        }

        Ok(())
    }

    pub fn status(&self)
                  -> FroyoResult<FroyoStatus> {
        let mut degraded = 0;
        for rd in self.raid_devs.values() {
            let rd = RefCell::borrow(rd);
            let (r_status, _) = try!(rd.status());
            match r_status {
                RaidStatus::Failed => return Ok(FroyoStatus::RaidFailed),
                RaidStatus::Degraded(_) => degraded += 1,
                RaidStatus::Good => {},
            }
        }

        if let ThinPoolStatus::Fail = try!(self.thin_pool_dev.status()) {
            return Ok(FroyoStatus::ThinPoolFailed)
        }

        if let ThinStatus::Fail = try!(self.thin_devs[0].status()) {
            return Ok(FroyoStatus::ThinFailed)
        }

        let working_status = {
            if degraded != 0 {
                FroyoRunningStatus::Degraded(degraded)
            } else if self.throttled {
                FroyoRunningStatus::Throttled
            } else {
                FroyoRunningStatus::Good
            }
        };

        Ok(FroyoStatus::Good(working_status))
    }

    // Get how much RAIDed space there is available. This consists of
    // unused blocks in the thin pool data area, and we also include
    // unused RAID space, since we could extend the data area if
    // needed. (The reason we don't is that currently there is no way
    // to shrink the size of the data area, so keeping it unallocated
    // if possible may give us more leeway in reshaping smaller.)
    //
    pub fn avail_redundant_space(&self) -> FroyoResult<Sectors> {
        let raid_avail = {
            self.raid_devs.values()
                .map(|rd| RefCell::borrow(rd).avail_sectors())
                .sum::<Sectors>()
        };

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

    pub fn total_redundant_space(&self) -> Sectors {
        self.raid_devs.values()
            .map(|rd| RefCell::borrow(rd).length)
            .sum::<Sectors>()
    }

    pub fn data_block_size(&self) -> u64 {
        self.thin_pool_dev.data_block_size()
    }

    fn get_raid_segments(sectors: Sectors,
                         raid_devs: &BTreeMap<String, Rc<RefCell<RaidDev>>>)
                         -> Option<Vec<RaidSegment>> {
        let mut needed = sectors;
        let mut segs = Vec::new();
        for rd in raid_devs.values() {
            if needed == Sectors::new(0) {
                break
            }
            let (gotten, r_segs) = RefCell::borrow(rd).get_some_space(needed);
            segs.extend(r_segs.iter()
                        .map(|&(start, len)| RaidSegment::new(start, len, rd)));
            needed = needed - gotten;
        }

        match *needed {
            0 => Some(segs),
            _ => None,
        }
    }

    pub fn extend_thinpool_data_dev(&mut self, length: Sectors) -> FroyoResult<()> {
        let new_segs = try!(Self::get_raid_segments(
            length, &self.raid_devs).ok_or_else(||
            io::Error::new(io::ErrorKind::InvalidInput,
                           "no space for extending thinpool data")));

        dbgp!("Extending tpool data dev by {}", *length);
        try!(self.thin_pool_dev.extend_data_dev(new_segs));

        Ok(())
    }

    pub fn extend_thinpool_meta_dev(&mut self, length: Sectors) -> FroyoResult<()> {
        let new_segs = try!(Self::get_raid_segments(
            length, &self.raid_devs).ok_or_else(||
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
            let total = self.total_redundant_space();
            try!(DbusContext::update_one(&dc.total_prop, (*total).into()));

            let (status, r_status): (u32,u32) = match try!(self.status()) {
                FroyoStatus::RaidFailed => (0x100, 0),
                FroyoStatus::ThinPoolFailed => (0x200, 0),
                FroyoStatus::ThinFailed => (0x400, 0),
                FroyoStatus::Good(rs) => match rs {
                    FroyoRunningStatus::Degraded(x) => (0, x as u32),
                    FroyoRunningStatus::Throttled => (0, 0x800),
                    FroyoRunningStatus::Good => (0, 0),
                },
            };
            try!(DbusContext::update_one(&dc.status_prop, status.into()));
            try!(DbusContext::update_one(&dc.running_status_prop, r_status.into()));

           let bdev_msg = DbusContext::get_block_devices_msgitem(&self.block_devs);
           try!(DbusContext::update_one(&dc.block_devices_prop, bdev_msg));
        }
        Ok(())
    }

    fn add_new_block_device(&mut self, blockdev: &Rc<RefCell<BlockDev>>)
                                -> FroyoResult<()> {
        let dm = try!(DM::new());

        // let existing raids know about the new disk, maybe they're degraded
        for (_, raid) in &mut self.raid_devs {
            let mut raid = RefCell::borrow_mut(&raid);
            try!(raid.new_block_device_added(&dm, &self.id, &blockdev));
        }

        Ok(())
    }

    fn add_existing_block_device(&mut self, blockdev: &Rc<RefCell<BlockDev>>)
                                 -> FroyoResult<()> {
        let dm = try!(DM::new());

        for (_, raid) in &mut self.raid_devs {
            let mut raid = RefCell::borrow_mut(&raid);
            try!(raid.block_device_found(&dm, &self.id, &blockdev));
        }

        Ok(())
    }

    pub fn add_block_device(&mut self, path: &Path, force: bool) -> FroyoResult<()> {
        let bd = {
            match BlockDev::setup(path) {
                Ok(found_bd) => {
                    // Does the new blockdev's froyo id match us?
                    if found_bd.froyodev_id == self.id {
                        if self.block_devs.contains_key(&found_bd.id) {
                            if let BlockMember::Present(_) =
                                *self.block_devs.get(&found_bd.id).unwrap() {
                                    return Err(FroyoError::Froyo(InternalError(
                                        format!("Block member {} already present \
                                                 in froyodev {}",
                                                found_bd.id, self.name))))
                                }

                            // Known but absent
                            let found_bd = Rc::new(RefCell::new(found_bd));
                            try!(self.add_existing_block_device(&found_bd));
                            found_bd
                        } else {
                            dbgp!("Block device {} mistakenly believes \
                                   it's part of froyodev {}, adding as new",
                                  path.display(), self.name);
                            let new_bd = Rc::new(RefCell::new(
                                try!(BlockDev::new(&self.id, path, force))));
                            try!(self.add_new_block_device(&new_bd));
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
                                    froyo_save.id))));
                    }
                },
                Err(_) => {
                    // setup() failed, so blockdev is not a current
                    // froyo member disk. Initialize and add it.
                    let bd = Rc::new(RefCell::new(
                        try!(BlockDev::new(&self.id, path, force))));
                    try!(self.add_new_block_device(&bd));
                    bd
                }
            }
        };

        // Depending on the above, we either insert or update an
        // existing entry
        self.block_devs.insert(
            RefCell::borrow(&bd).id.clone(),
            BlockMember::Present(bd.clone()));

        Ok(())
    }

    // If removing blockdev would break a raid in the Froyodev, fail
    //
    // If blockdev is used by raids but they can continue degraded,
    // reload raids without LinearDevs from the blockdev
    //
    // Remove blockdev from self.block_devs, wipe superblock
    pub fn remove_block_device(&mut self, path: &Path) -> FroyoResult<()> {

        // Lookup the blockdev to remove.
        // NOTE: This blockdev is a copy of an item in self.block_devs
        // except its linear_devs is empty
        let mut blockdev = try!(BlockDev::setup(path));

        // Check this blockdev is in this froyodev
        if !self.block_devs.contains_key(&blockdev.id) {
            return Err(FroyoError::Froyo(InternalError(
                format!("{} is not a member of {}",
                        path.display(), self.name))))
        }

        // We could get affected lineardevs from looking at
        // self.block_devs[x].linear_devs but there's no backlink from
        // lineardevs to raids (and hard to add).
        // Instead, do it top-down.
        let raids_and_linears_using_dev = self.raid_devs.iter_mut()
            .map(|(_, rd)| RefCell::borrow_mut(rd))
            .filter_map(|rd| {
                let mut ld_index = None;
                for (count, rm) in rd.members.iter().enumerate() {
                    if let Some(pres) = rm.present() {
                        let ld = RefCell::borrow(&pres);
                        let parent_bd = RefCell::borrow(&ld.parent);
                        if parent_bd.dev == blockdev.dev {
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
                                path.display()))))
                },
            }
            match action {
                RaidAction::Idle => {},
                x => {
                    return Err(FroyoError::Froyo(InternalError(
                        format!("Cannot remove {}, a RAID is in state {:?}",
                                path.display(), x))))
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

                // ..put in Removed value.
                let mut borrowed_ld = RefCell::borrow_mut(&ld);
                raid.members[ld_idx] = RaidMember::Removed;

                // TODO panic if reload fails???
                try!(raid.reload(&dm, None));
                try!(borrowed_ld.teardown(&dm));
            }
        }

        self.block_devs.remove(&blockdev.id)
            .expect("blockdev should always still be here for us to remove");
        try!(blockdev.wipe_mda_header());

        Ok(())
    }

    pub fn reshape(&mut self) -> FroyoResult<()> {
        self.reshaping = true;
        // Summary:
        // phase 1: get redundant
        // phase 2: use all space efficiently
        //
        // thinpool extend needed while reshape? cancel reshape. (how?)
        //
        // phase 1:
        // for each degraded raiddev with data:
        //   ** try to copy data to non-degraded raids **
        //   make a mirror (no metadata) on top of old & new raiddev
        //   suspend thinpool
        //   change thinpool meta/data to use mirror dev
        //   resume thinpool
        //   wait until synced
        //   suspend thinpool
        //   remove the mirror
        //   point thinpool raidseg at new raiddev
        //   save state
        //   resume thinpool
        //   blow away original raiddev and recreate for future use

        // for each degraded raiddev still with data:
        //   ** use scratch space **
        //   clear scratch space enough for used data areas
        //   make a linear vol in scratch space the same size as used data
        //   suspend thinpool
        //   make a mirror (no metadata) with the raiddev and linear vol
        //   change thinpool meta/data to use mirror dev
        //   resume thinpool
        //   wait until synced
        //   suspend thinpool
        //   remove the orig leg from the mirror
        //   save state, as degraded mirror on the linear dev
        //   resume thinpool
        //   blow away and recreate original raiddev (may get bigger or smaller)
        //   suspend thinpool
        //   remove 1st mirror
        //   make another mirror across the linear dev and the raiddev
        //     and update thinpool table
        //   save state, raid 1-on-5 (new thing in json? restore
        //     properly on reboot?) in-between raid and thinpool layer?
        //   resume thinpool
        //   wait until synced
        //   suspend thinpool
        //   remove mirror, update thinpool to target raiddev
        //   save state
        //   resume thinpool

        // what if more data on degraded raid that will fit in new
        // raid?  then there must be another raid with space or
        // can_reshape() would've failed.
        // not having room for data in each iter would be a PITA.
        // solution: do raids in most-sectors-free descending order?

        // we are now redundant!
        // phase 2: use space efficiently
        // TBD
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
    pub fn can_reshape(&self) -> bool {
        // Just one disk, no way we can re-establish redundancy
        if self.block_devs.iter()
            .filter_map(|(_, bd)| bd.present())
            .count() < 2 {
                return false
            }

        // scratch space must be greater than largest used space in
        // any degraded raid (so we can make a temp copy)
        // scratch is unused space plus space from unused raids
        let unused_space = self.block_devs.iter()
            .filter_map(|(_, bd)| bd.present())
            .flat_map(|bd| RefCell::borrow(&*bd).avail_areas().into_iter()
                      .map(|(_, len)| len))
            .sum::<Sectors>();

        let unused_raid_space = self.raid_devs.iter()
            .filter(|&(_, rd)| !RefCell::borrow(rd).used_areas().is_empty())
            .map(|(_, rd)| {
                let rd = RefCell::borrow(rd);
                let (memb_meta_size, memb_data_size) = rd.per_member_size().unwrap();
                let memb_tot_size = memb_meta_size + memb_data_size;
                let members_present = rd.members.iter()
                    .filter_map(|rm| rm.present())
                    .count();
                Sectors::new(members_present as u64 * *memb_tot_size)
            })
            .sum::<Sectors>();

        let max_used_raid_sectors = self.raid_devs.iter()
            .map(|(_, rd)| {
                let rd = RefCell::borrow(rd);
                rd.used_areas().into_iter()
                    .map(|(_, len)| len)
                    .sum::<Sectors>()
            })
            .max().unwrap_or_else(|| Sectors::new(0));

        if max_used_raid_sectors > (unused_space + unused_raid_space) {
            return false
        }

        let reshaped_tot_size = self.raid_devs.iter()
            .map(|(_, rd)| {
                let rd = RefCell::borrow(rd);
                let per_member_data_size = rd.per_member_size().unwrap().1;
                let members_present = rd.members.iter()
                    .filter_map(|rm| rm.present())
                    .count();
                let sz = (members_present - REDUNDANCY) * *per_member_data_size as usize;
                Sectors::new(sz as u64)
            })
            .sum::<Sectors>();

        if self.thin_pool_dev.used_sectors() > reshaped_tot_size {
            return false
        }

        true
    }

    pub fn check_status(&mut self) -> FroyoResult<()> {
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
                            let meta_sectors = self.thin_pool_dev.meta_dev.length();
                            try!(self.extend_thinpool_meta_dev(meta_sectors));
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

        dbgp!("Block devs: {}", self.block_devs.len());
        for bm in self.block_devs.values() {
            match *bm {
                BlockMember::Present(ref bd) => {
                    let bd = RefCell::borrow(&bd);
                    dbgp!("  dev {} largest avail {}",
                          bd.path.display(),
                          bd.largest_avail_area().map(|(_, x)| *x).unwrap_or(0u64));
                },
                BlockMember::Absent(ref bds) => {
                    dbgp!("  dev {} absent", bds.path.display());
                },
            }
        }

        for (raid_count, raid) in self.raid_devs.values().enumerate() {
            let raid = RefCell::borrow(raid);
            let (status, action) = try!(raid.status());
            dbgp!("Raid dev #{} status {:?} action {:?}", raid_count, status, action);
            for (leg_count, rm) in raid.members.iter().enumerate() {
                match *rm {
                    RaidMember::Present(ref ld) => {
                        let ld = RefCell::borrow(&ld);
                        dbgp!("  #{} size {}", leg_count, *ld.data_length());
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
        dbgp!("thin pool meta on {} raids", self.thin_pool_dev.meta_dev.parents().len());
        dbgp!("thin pool data on {} raids", self.thin_pool_dev.data_dev.parents().len());

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
