// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::io;
use std::io::ErrorKind;
use std::cmp::min;
use std::fmt;
use std::mem;

use devicemapper::{DM, Device, DmFlags, DevId, DM_SUSPEND};

use types::{Sectors, SectorOffset, FroyoError, FroyoResult};
use blockdev::{LinearDev, LinearDevSave, BlockDev, LinearSegment};
use consts::*;
use util::{setup_dm_dev, teardown_dm_dev};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaidDevSave {
    pub stripe_sectors: Sectors,
    pub region_sectors: Sectors,
    pub length: Sectors,
    pub member_count: usize,
    pub members: BTreeMap<String, LinearDevSave>,
}

#[derive(Debug, Clone)]
pub struct RaidDev {
    pub id: String,
    dev: Device,
    dm_name: String,
    pub stripe_sectors: Sectors,
    pub region_sectors: Sectors,
    pub length: Sectors,
    pub members: Vec<RaidMember>,
    used: BTreeMap<SectorOffset, Sectors>,
}

#[derive(Debug, Clone, Copy)]
pub enum RaidStatus {
    Good,
    Degraded(usize),
    Failed,
}

#[derive(Debug, Clone, Copy)]
pub enum RaidAction {
    Idle,
    Frozen,
    Resync,
    Recover,
    Check,
    Repair,
    Reshape,
    Unknown,
}

#[derive(Debug, Clone)]
pub enum RaidMember {
    Present(Rc<RefCell<LinearDev>>),
    Absent((String, LinearDevSave)),
    Removed,
}

impl RaidMember {
    pub fn present(&self) -> Option<Rc<RefCell<LinearDev>>> {
        match *self {
            RaidMember::Present(ref x) => Some(x.clone()),
            RaidMember::Absent(_) => None,
            RaidMember::Removed => None,
        }
    }
}

impl RaidDev {

    fn make_raid_params(
        devs: &[RaidMember],
        stripe: Sectors,
        region: Sectors,
        rebuild: Option<usize>)
        -> String {

        let raid_texts: Vec<_> = devs.iter()
            .map(|dev|
                 match *dev {
                     RaidMember::Present(ref dev) => {
                         format!("{}:{} {}:{}",
                                 RefCell::borrow(dev).meta_dev.major,
                                 RefCell::borrow(dev).meta_dev.minor,
                                 RefCell::borrow(dev).data_dev.major,
                                 RefCell::borrow(dev).data_dev.minor)
                     },
                     RaidMember::Absent(_) => "- -".to_owned(),
                     RaidMember::Removed => "- -".to_owned(),
                 })
            .collect();

        match rebuild {
            None => format!("raid5_ls 3 {} region_size {} {} {}",
                            *stripe,
                            *region,
                            raid_texts.len(),
                            raid_texts.join(" ")),
            Some(idx) => format!("raid5_ls 5 {} region_size {} rebuild {} {} {}",
                                 *stripe,
                                 *region,
                                 idx,
                                 raid_texts.len(),
                                 raid_texts.join(" ")),
        }
    }

    pub fn setup(dm: &DM, name: &str, id: String, devs: Vec<RaidMember>,
                 stripe: Sectors, region: Sectors)
                 -> FroyoResult<RaidDev> {
        let present_devs = devs.iter().filter_map(|ref x| x.present()).count();
        if present_devs < (devs.len() - REDUNDANCY) {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!(
                    "Too many missing devs to create raid: {}. Need at least {} of {}",
                    devs.len() - present_devs, devs.len() - REDUNDANCY,
                    devs.len()))))
        }

        let first_present_dev = devs.iter()
            .filter_map(|ref x| x.present())
            .next()
            .unwrap();
        let first_present_dev_len = first_present_dev.borrow().data_length();

        // Verify all present devs are the same length
        if !devs.iter().filter_map(|x| x.present()).all(
            |x| x.borrow().data_length() == first_present_dev_len) {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput, "RAID member device sizes differ")))
        }

        let target_length = first_present_dev_len
            * Sectors((devs.len() - REDUNDANCY) as u64);

        let params = Self::make_raid_params(&devs, stripe, region, None);
        let raid_table = [(0u64, *target_length, "raid", params)];
        let dm_name = format!("froyo-raid5-{}-{}", name, id);
        let raid_dev = try!(setup_dm_dev(dm, &dm_name, &raid_table));

        Ok(RaidDev {
            id: id,
            dev: raid_dev,
            dm_name: dm_name,
            stripe_sectors: stripe,
            region_sectors: region,
            length: target_length,
            members: devs,
            used: BTreeMap::new(),
        })
    }

    pub fn teardown(&mut self, dm: &DM) -> FroyoResult<()> {
        try!(teardown_dm_dev(dm, &self.dm_name));
        for member in &self.members {
            if let RaidMember::Present(ref linear) = *member {
                try!(RefCell::borrow_mut(linear).teardown(dm))
            }
        }

        Ok(())
    }

    pub fn reload(&mut self, dm: &DM, rebuild: Option<usize>) -> FroyoResult<()> {
        let params = Self::make_raid_params(
            &self.members, self.stripe_sectors, self.region_sectors, rebuild);
        let raid_table = [(0u64, *self.length, "raid", params)];
        let id = &DevId::Name(&self.dm_name);

        try!(dm.table_load(id, &raid_table));
        try!(dm.device_suspend(id, DM_SUSPEND));
        try!(dm.device_suspend(id, DmFlags::empty()));

        Ok(())
    }

    pub fn to_save(&self) -> RaidDevSave {
        RaidDevSave {
            stripe_sectors: self.stripe_sectors,
            region_sectors: self.region_sectors,
            length: self.length,
            member_count: self.members.len(),
            members: self.members.iter().enumerate()
                .filter_map(|(position, dev)| {
                    match *dev {
                        RaidMember::Present(ref x) =>
                            Some((position.to_string(), RefCell::borrow(x).to_save())),
                        RaidMember::Absent((_, ref sld)) =>
                            Some((position.to_string(), sld.clone())),
                        RaidMember::Removed => None,
                    }
                })
                .collect(),
        }
    }

    pub fn used_areas(&self)-> Vec<(SectorOffset, Sectors)> {
        self.used.iter()
            .map(|(key, val)| {
                (*key, *val)
            })
            .collect()
    }

    fn avail_areas(&self) -> Vec<(SectorOffset, Sectors)> {
        let mut used_vec = self.used_areas();

        used_vec.sort();
        // Insert an entry to mark the end of the raiddev so the fold works
        // correctly
        used_vec.push((SectorOffset(*self.length), Sectors(0)));

        let mut avail_vec = Vec::new();
        used_vec.iter()
            .fold(SectorOffset(0), |prev_end, &(start, len)| {
                if prev_end < start {
                    avail_vec.push((prev_end, Sectors(*start-*prev_end)));
                }
                start + SectorOffset(*len)
            });

        avail_vec
    }

    pub fn avail_sectors(&self) -> Sectors {
        self.avail_areas().into_iter()
            .map(|(_, len)| len)
            .sum()
    }

    // Find some sector ranges that could be allocated. If more
    // sectors are needed than our capacity, return partial results.
    pub fn get_some_space(&self, size: Sectors) -> (Sectors, Vec<(SectorOffset, Sectors)>) {
        let mut segs = Vec::new();
        let mut needed = size;

        for (start, len) in self.avail_areas() {
            if needed == Sectors(0) {
                break
            }

            let to_use = min(needed, len);

            segs.push((start, to_use));
            needed = needed - to_use;
        }

        (size - needed, segs)
    }

    pub fn status(&self) -> FroyoResult<(RaidStatus, RaidAction)> {
        let dm = try!(DM::new());

        let (_, mut status) = try!(dm.table_status(&DevId::Name(&self.dm_name), DmFlags::empty()));

        if status.len() != 1 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Expected 1 line from raid status")))
        }

        // See kernel's dm-raid.txt "Status Output"
        let status_line = status.pop().unwrap().3;
        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        if status_vals.len() < 5 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Kernel returned too few values from raid status")))
        }

        let mut bad = 0;
        for c in status_vals[2].chars() {
            match c {
                'A' => {},
                'a' => {},
                'D' => bad += 1,
                x => return Err(FroyoError::Io(io::Error::new(
                    ErrorKind::InvalidData,
                    format!("Kernel returned unknown raid health char '{}'", x)))),
            }
        }

        // Status characters indicate if a drive is faulty, but not if
        // the raid was configured with missing devices. Add them.
        bad += self.members.iter().
            filter(|rm| rm.present().is_none())
            .count();

        let raid_status = match bad {
            0 => RaidStatus::Good,
            x @ 1...REDUNDANCY => RaidStatus::Degraded(x),
            _ => RaidStatus::Failed,
        };

        let raid_action = match status_vals[4] {
            "idle" => RaidAction::Idle,
            "frozen" => RaidAction::Frozen,
            "resync" => RaidAction::Resync,
            "recover" => RaidAction::Recover,
            "check" => RaidAction::Check,
            "repair" => RaidAction::Repair,
            "reshape" => RaidAction::Reshape,
            _ => RaidAction::Unknown,
        };

        Ok((raid_status, raid_action))
    }

    pub fn per_member_size(&self) -> Option<(Sectors, Sectors)> {
        // all members should be the same size
        if let Some(ld) = self.members.iter()
            .filter_map(|rm| rm.present())
            .next() {
                let ld = RefCell::borrow(&ld);
                return Some((ld.metadata_length(), ld.data_length()));
            }

        None
    }

    // A new block device has been added. Are we degraded?
    // Try to use it!
    pub fn new_block_device_added(
        &mut self,
        dm: &DM,
        froyo_id: &str,
        blockdev: &Rc<RefCell<BlockDev>>)
        -> FroyoResult<()> {

        let (meta_spc, data_spc) = self.per_member_size().unwrap();

        // get index of slot we should fill
        let idx = self.members.iter().enumerate()
            .filter(|&(_, ref rm)| rm.present().is_none())
            .map(|(idx, _)| idx)
            .next();

        if let Some(idx) = idx {
            let needed = meta_spc + data_spc;
            let (offset, len) = RefCell::borrow(blockdev).largest_avail_area()
                .unwrap_or((SectorOffset(0), Sectors(0)));
            if len >= needed {
                let linear = Rc::new(RefCell::new(try!(LinearDev::new(
                    &dm,
                    &format!("{}-{}-{}", froyo_id, self.id, idx),
                    blockdev,
                    &[LinearSegment {
                        start: offset,
                        length: meta_spc,
                    }],
                    &[LinearSegment {
                        start: offset + SectorOffset(*meta_spc),
                        length: data_spc,
                    }]))));
                blockdev.borrow_mut().linear_devs.push(linear.clone());
                self.members[idx] = RaidMember::Present(linear);

                try!(self.reload(dm, Some(idx)));
            }
        }

        Ok(())
    }

    // If there is an absent member that matches the id of the
    // just-found block device, set it up, mark it present, and reload
    // the raid.
    pub fn block_device_found(
        &mut self,
        dm: &DM,
        froyo_id: &str,
        blockdev: &Rc<RefCell<BlockDev>>)
        -> FroyoResult<()> {

        let res = self.members.iter().enumerate()
            .filter_map(|(idx, rm)| {
                match *rm {
                    RaidMember::Present(_) => None,
                    RaidMember::Absent(ref sld_tuple) => Some((idx, sld_tuple.clone())),
                    RaidMember::Removed => None,
                }
            })
            .filter(|&(_, (ref id, _))| *id == RefCell::borrow(blockdev).id)
            .next();

        if let Some((idx, (_, sld))) = res {
            let linear = Rc::new(RefCell::new(try!(LinearDev::setup(
                &dm,
                &format!("{}-{}-{}", froyo_id, self.id, idx),
                &blockdev,
                &sld.meta_segments,
                &sld.data_segments))));
            RefCell::borrow_mut(blockdev).linear_devs.push(linear.clone());
            self.members[idx] = RaidMember::Present(linear);

            try!(self.reload(dm, None));
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaidSegmentSave {
    pub start: SectorOffset,
    pub length: Sectors,
    pub parent: String,  // RaidDev id
}

#[derive(Clone)]
pub struct RaidSegment {
    pub start: SectorOffset,
    pub length: Sectors,
    pub parent: Rc<RefCell<RaidDev>>,
}

impl fmt::Debug for RaidSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}, {})", *self.start, *self.length,
               RefCell::borrow(&self.parent).id)
    }
}

impl RaidSegment {
    pub fn new(start: SectorOffset, length: Sectors, parent: &Rc<RefCell<RaidDev>>)
               -> RaidSegment {
        RefCell::borrow_mut(parent).used.insert(start, length);
        RaidSegment {
            start: start,
            length: length,
            parent: parent.clone(),
        }
    }

    // Also update the used map over in self.parent.used.
    fn update_length(&mut self, length: Sectors) {
        let mut parent = RefCell::borrow_mut(&self.parent);
        let entry = parent.used.get_mut(&self.start).unwrap();
        mem::replace(entry, length);
        self.length = length;
    }

    pub fn to_save(&self) -> RaidSegmentSave {
        RaidSegmentSave {
            start: self.start,
            length: self.length,
            parent: RefCell::borrow(&self.parent).id.clone(),
        }
    }
}

impl Drop for RaidSegment {
    fn drop(&mut self) {
        self.parent.borrow_mut().used.remove(&self.start);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaidLinearDevSave {
    pub id: String,
    pub segments: Vec<RaidSegmentSave>,
}

#[derive(Debug, Clone)]
pub struct RaidLinearDev {
    id: String,
    pub dm_name: String,
    pub dev: Device,
    pub segments: Vec<RaidSegment>,
}

impl RaidLinearDev {
    pub fn dm_table(segments: &[RaidSegment])
                    -> Vec<(u64, u64, String, String)> {
        let mut table = Vec::new();
        let mut offset = SectorOffset(0);
        for seg in segments {
            let line = (*offset, *seg.length, "linear".to_owned(),
                        format!("{}:{} {}",
                                RefCell::borrow(&seg.parent).dev.major,
                                RefCell::borrow(&seg.parent).dev.minor,
                                *seg.start));
            table.push(line);
            offset = offset + SectorOffset(*seg.length);
        }

        table
    }

    pub fn new(dm: &DM, name: &str, id: &str,
               segments: Vec<RaidSegment>)
               -> FroyoResult<RaidLinearDev> {
        Self::setup(dm, name, id, segments)
    }

    pub fn setup(dm: &DM, name: &str, id: &str,
                  segments: Vec<RaidSegment>)
              -> FroyoResult<RaidLinearDev> {

        let table = Self::dm_table(&segments);
        let dm_name = format!("froyo-raid-linear-{}", name);
        let linear_dev = try!(setup_dm_dev(dm, &dm_name, &table));

        Ok(RaidLinearDev {
            id: id.to_owned(),
            dm_name: dm_name,
            dev: linear_dev,
            segments: segments,
        })
    }

    pub fn teardown(&mut self, dm: &DM) -> FroyoResult<()> {
        teardown_dm_dev(dm, &self.dm_name)
    }

    pub fn to_save(&self) -> RaidLinearDevSave {
        RaidLinearDevSave {
            id: self.id.clone(),
            segments: self.segments.iter()
                .map(|x| x.to_save())
                .collect()
        }
    }

    pub fn length(&self) -> Sectors {
        self.segments.iter().map(|x| x.length).sum()
    }

    pub fn extend(&mut self, segs: Vec<RaidSegment>)
        -> FroyoResult<()> {

        // last existing and first new may be contiguous
        let coalesced_new_first = {
            let mut old_last = self.segments.last_mut().unwrap();
            let new_first = segs.first().unwrap();
            let mut coal = false;
            if old_last.parent.borrow().id == new_first.parent.borrow().id
                && (old_last.start + SectorOffset(*old_last.length)
                    == new_first.start) {
                    let new_len = old_last.length + new_first.length;
                    old_last.update_length(new_len);
                    coal = true;
                }
            coal
        };

        if coalesced_new_first {
            self.segments.extend(segs.into_iter().skip(1));
        } else {
            self.segments.extend(segs);
        }

        let table = RaidLinearDev::dm_table(&self.segments);

        let dm = try!(DM::new());
        let id = &DevId::Name(&self.dm_name);

        try!(dm.table_load(id, &table));
        try!(dm.device_suspend(id, DM_SUSPEND));
        try!(dm.device_suspend(id, DmFlags::empty()));

        Ok(())
    }

    pub fn parents(&self) -> BTreeMap<String, Rc<RefCell<RaidDev>>> {
        let mut map = BTreeMap::new();

        for rs in &self.segments {
            let id = RefCell::borrow(&*rs.parent).id.clone();
            map.insert(id, rs.parent.clone());
        }

        map
    }
}
