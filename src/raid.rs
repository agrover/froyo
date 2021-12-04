// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::fmt;
use std::io;
use std::io::ErrorKind;
use std::rc::Rc;

use devicemapper::DM;
use uuid::Uuid;

use blockdev::{BlockDev, BlockDevs, BlockMember, LinearDev, LinearDevSave, LinearSegment};
use consts::*;
use dmdevice::DmDevice;
use froyo::FroyoSave;
use mirror::TempDev;
use types::{FroyoError, FroyoResult, InternalError, SectorOffset, Sectors, SumSectors};
use util::align_to;

pub use serialize::{RaidDevSave, RaidLinearDevSave, RaidSegmentSave};

#[derive(Debug, Clone, PartialEq)]
pub struct RaidDev {
    pub id: String,
    pub dev: DmDevice,
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

#[derive(Debug, Clone, PartialEq)]
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
        rebuild: Option<usize>,
    ) -> String {
        let raid_texts: Vec<_> = devs
            .iter()
            .map(|dev| match *dev {
                RaidMember::Present(ref dev) => {
                    format!(
                        "{} {}",
                        dev.borrow().meta_dev.dstr(),
                        dev.borrow().data_dev.dstr()
                    )
                }
                RaidMember::Absent(_) => "- -".to_owned(),
                RaidMember::Removed => "- -".to_owned(),
            })
            .collect();

        match rebuild {
            None => format!(
                "raid5_ls 3 {} region_size {} {} {}",
                *stripe,
                *region,
                raid_texts.len(),
                raid_texts.join(" ")
            ),
            Some(idx) => format!(
                "raid5_ls 5 {} region_size {} rebuild {} {} {}",
                *stripe,
                *region,
                idx,
                raid_texts.len(),
                raid_texts.join(" ")
            ),
        }
    }

    pub fn setup(
        dm: &DM,
        name: &str,
        id: String,
        devs: Vec<RaidMember>,
        stripe: Sectors,
        region: Sectors,
    ) -> FroyoResult<RaidDev> {
        let present_devs = devs.iter().filter_map(|ref x| x.present()).count();
        if present_devs < (devs.len() - REDUNDANCY) {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!(
                    "Too many missing devs to create raid: {}. Need at least {} of {}",
                    devs.len() - present_devs,
                    devs.len() - REDUNDANCY,
                    devs.len()
                ),
            )));
        }

        let first_present_dev = devs.iter().filter_map(|ref x| x.present()).next().unwrap();
        let first_present_dev_len = first_present_dev.borrow().data_length();

        // Verify all present devs are the same length
        if !devs
            .iter()
            .filter_map(|x| x.present())
            .all(|x| x.borrow().data_length() == first_present_dev_len)
        {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                "RAID member device sizes differ",
            )));
        }

        let target_length = first_present_dev_len * Sectors((devs.len() - REDUNDANCY) as u64);

        let params = Self::make_raid_params(&devs, stripe, region, None);
        let raid_table = [(0u64, *target_length, "raid", params)];
        let dm_name = format!("froyo-raid5-{}-{}", name, id);
        let raid_dev = DmDevice::new(dm, &dm_name, &raid_table)?;

        Ok(RaidDev {
            id: id,
            dev: raid_dev,
            stripe_sectors: stripe,
            region_sectors: region,
            length: target_length,
            members: devs,
            used: BTreeMap::new(),
        })
    }

    pub fn teardown(&mut self, dm: &DM) -> FroyoResult<()> {
        self.dev.teardown(dm)?;
        for member in &self.members {
            if let RaidMember::Present(ref linear) = *member {
                linear.borrow_mut().teardown(dm)?
            }
        }

        Ok(())
    }

    pub fn destroy(&mut self, dm: &DM) -> FroyoResult<()> {
        self.dev.teardown(dm)?;
        for member in &self.members {
            if let RaidMember::Present(ref linear) = *member {
                let linear = linear.borrow_mut();
                linear.teardown(dm)?;
                let bd = linear.parent.upgrade().unwrap();
                bd.borrow_mut().linear_devs.remove(&linear.meta_dev.dm_name);
            }
        }

        Ok(())
    }

    pub fn reload(&mut self, dm: &DM, rebuild: Option<usize>) -> FroyoResult<()> {
        let params = Self::make_raid_params(
            &self.members,
            self.stripe_sectors,
            self.region_sectors,
            rebuild,
        );
        let raid_table = [(0u64, *self.length, "raid", params)];
        self.dev.reload(dm, &raid_table)?;

        Ok(())
    }

    pub fn to_save(&self) -> RaidDevSave {
        RaidDevSave {
            stripe_sectors: self.stripe_sectors,
            region_sectors: self.region_sectors,
            length: self.length,
            member_count: self.members.len(),
            members: self
                .members
                .iter()
                .enumerate()
                .filter_map(|(position, dev)| match *dev {
                    RaidMember::Present(ref x) => {
                        Some((position.to_string(), x.borrow().to_save()))
                    }
                    RaidMember::Absent((_, ref sld)) => Some((position.to_string(), sld.clone())),
                    RaidMember::Removed => None,
                })
                .collect(),
        }
    }

    fn used_areas(&self) -> Vec<(SectorOffset, Sectors)> {
        self.used.iter().map(|(key, val)| (*key, *val)).collect()
    }

    fn avail_areas(&self) -> Vec<(SectorOffset, Sectors)> {
        let mut used_vec = self.used_areas();

        used_vec.sort();
        // Insert an entry to mark the end of the raiddev so the fold works
        // correctly
        used_vec.push((SectorOffset(*self.length), Sectors(0)));

        let mut avail_vec = Vec::new();
        used_vec
            .iter()
            .fold(SectorOffset(0), |prev_end, &(start, len)| {
                if prev_end < start {
                    avail_vec.push((prev_end, Sectors(*start - *prev_end)));
                }
                start + SectorOffset(*len)
            });

        avail_vec
    }

    fn avail_sectors(&self) -> Sectors {
        self.avail_areas()
            .into_iter()
            .map(|(_, len)| len)
            .sum_sectors()
    }

    // Is this raiddev a good one to maybe put more stuff on?
    pub fn is_safe(&self) -> bool {
        self.members.iter().all(|rm| rm.present().is_some())
    }

    pub fn is_empty(&self) -> bool {
        self.used.is_empty()
    }

    // Find some sector ranges that could be allocated. If more
    // sectors are needed than our capacity, return partial results.
    pub fn get_some_space(&self, size: Sectors) -> (Sectors, Vec<(SectorOffset, Sectors)>) {
        let mut segs = Vec::new();
        let mut needed = size;

        for (start, len) in self.avail_areas() {
            if needed == Sectors(0) {
                break;
            }

            let to_use = min(needed, len);

            segs.push((start, to_use));
            needed = needed - to_use;
        }

        (size - needed, segs)
    }

    pub fn status(&self) -> FroyoResult<(RaidStatus, RaidAction)> {
        let dm = DM::new()?;

        let mut status = self.dev.table_status(&dm)?;

        if status.len() != 1 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Expected 1 line from raid status",
            )));
        }

        // See kernel's dm-raid.txt "Status Output"
        let status_line = status.pop().unwrap().3;
        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        if status_vals.len() < 5 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Kernel returned too few values from raid status",
            )));
        }

        let mut bad = 0;
        for c in status_vals[2].chars() {
            match c {
                'A' => {}
                'a' => {}
                'D' => bad += 1,
                x => {
                    return Err(FroyoError::Io(io::Error::new(
                        ErrorKind::InvalidData,
                        format!("Kernel returned unknown raid health char '{}'", x),
                    )))
                }
            }
        }

        // Status characters indicate if a drive is faulty, but not if
        // the raid was configured with missing devices. Add them.
        bad += self
            .members
            .iter()
            .filter(|rm| rm.present().is_none())
            .count();

        let raid_status = match bad {
            0 => RaidStatus::Good,
            x @ 1..=REDUNDANCY => RaidStatus::Degraded(x),
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
        if let Some(ld) = self.members.iter().filter_map(|rm| rm.present()).next() {
            let ld = ld.borrow();
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
        blockdev: &Rc<RefCell<BlockDev>>,
    ) -> FroyoResult<()> {
        let (meta_spc, data_spc) = self.per_member_size().unwrap();

        // get index of slot we should fill
        let idx = self
            .members
            .iter()
            .enumerate()
            .filter(|&(_, ref rm)| rm.present().is_none())
            .map(|(idx, _)| idx)
            .next();

        if let Some(idx) = idx {
            let needed = meta_spc + data_spc;
            let (offset, len) = blockdev
                .borrow()
                .largest_avail_area()
                .unwrap_or((SectorOffset(0), Sectors(0)));
            if len >= needed {
                let linear = Rc::new(RefCell::new(LinearDev::new(
                    &dm,
                    &format!("{}-{}-{}", froyo_id, self.id, idx),
                    blockdev,
                    &[LinearSegment::new(offset, meta_spc)],
                    &[LinearSegment::new(
                        offset + SectorOffset(*meta_spc),
                        data_spc,
                    )],
                )?));

                blockdev
                    .borrow_mut()
                    .linear_devs
                    .insert(linear.borrow().meta_dev.dm_name.clone(), linear.clone());
                self.members[idx] = RaidMember::Present(linear);

                self.reload(dm, Some(idx))?;
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
        blockdev: &Rc<RefCell<BlockDev>>,
    ) -> FroyoResult<()> {
        let res = self
            .members
            .iter()
            .enumerate()
            .filter_map(|(idx, rm)| match *rm {
                RaidMember::Present(_) => None,
                RaidMember::Absent(ref sld_tuple) => Some((idx, sld_tuple.clone())),
                RaidMember::Removed => None,
            })
            .filter(|&(_, (ref id, _))| *id == blockdev.borrow().id)
            .next();

        if let Some((idx, (_, sld))) = res {
            let linear = Rc::new(RefCell::new(LinearDev::setup(
                &dm,
                &format!("{}-{}-{}", froyo_id, self.id, idx),
                &blockdev,
                &sld.meta_segments,
                &sld.data_segments,
            )?));

            blockdev
                .borrow_mut()
                .linear_devs
                .insert(linear.borrow().meta_dev.dm_name.clone(), linear.clone());
            self.members[idx] = RaidMember::Present(linear);

            self.reload(dm, None)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct RaidDevs {
    pub raids: BTreeMap<String, Rc<RefCell<RaidDev>>>,

    // temp_dev is a linear mapping to non-redundant space. During a
    // reshape this may be present, and the saved configuration may
    // refer to it. Basically, when building RaidLinearDevs for the
    // thinpool meta and data devices, if the thinpooldev segment's
    // parent uuid doesn't reference a raiddev, we then check the
    // TempDev. If it's not in either then our saved info is corrupt.
    pub temp_dev: Option<Rc<RefCell<TempDev>>>,
}

impl RaidDevs {
    pub fn new(dm: &DM, name: &str, block_devs: &BlockDevs) -> FroyoResult<RaidDevs> {
        let mut raid_devs = RaidDevs {
            raids: BTreeMap::new(),
            temp_dev: None,
        };

        raid_devs.create_redundant_zones(dm, name, block_devs)?;

        Ok(raid_devs)
    }

    pub fn setup(dm: &DM, froyo_save: &FroyoSave, block_devs: &BlockDevs) -> FroyoResult<RaidDevs> {
        let mut raid_devs = RaidDevs {
            raids: BTreeMap::new(),
            temp_dev: None,
        };
        for (id, srd) in &froyo_save.raid_devs {
            let rd = Rc::new(RefCell::new(Self::setup_raiddev(
                &dm,
                &froyo_save.id,
                id,
                srd,
                block_devs,
            )?));
            let id = rd.borrow().id.clone();

            if let (RaidStatus::Failed, _) = rd.borrow().status()? {
                return Err(FroyoError::Froyo(InternalError(
                    format!("Froyodev {} has a failed raid", froyo_save.name).into(),
                )));
            }

            raid_devs.raids.insert(id, rd);
        }

        if let Some(td) = TempDev::setup(dm, froyo_save, block_devs)? {
            raid_devs.temp_dev = Some(Rc::new(RefCell::new(td)));
        }

        Ok(raid_devs)
    }

    fn setup_raiddev(
        dm: &DM,
        froyo_id: &str,
        raid_id: &str,
        raid_save: &RaidDevSave,
        block_devs: &BlockDevs,
    ) -> FroyoResult<RaidDev> {
        let mut linear_devs = Vec::new();

        // Loop through saved struct and setup legs if present in both
        // the blockdev list and the raid members list
        for count in 0..raid_save.member_count {
            match raid_save.members.get(&count.to_string()) {
                Some(sld) => match block_devs.0.get(&sld.parent) {
                    Some(bm) => match *bm {
                        BlockMember::Present(ref bd) => {
                            let ld = Rc::new(RefCell::new(LinearDev::setup(
                                &dm,
                                &format!("{}-{}-{}", froyo_id, raid_id, count),
                                &bd,
                                &sld.meta_segments,
                                &sld.data_segments,
                            )?));
                            bd.borrow_mut()
                                .linear_devs
                                .insert(ld.borrow().meta_dev.dm_name.clone(), ld.clone());
                            linear_devs.push(RaidMember::Present(ld));
                        }
                        BlockMember::Absent(_) => {
                            dbgp!("Expected device absent from raid {}", raid_id);
                            linear_devs.push(RaidMember::Absent((sld.parent.clone(), sld.clone())));
                        }
                    },
                    None => {
                        return Err(FroyoError::Io(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            format!(
                                "Invalid metadata, raiddev {} references \
                                         blockdev {} that is not found in \
                                         blockdev list",
                                raid_id, sld.parent
                            ),
                        )))
                    }
                },
                None => {
                    dbgp!("Raid {} member not present", raid_id);
                    linear_devs.push(RaidMember::Removed);
                }
            }
        }

        RaidDev::setup(
            dm,
            froyo_id,
            raid_id.to_owned(),
            linear_devs,
            raid_save.stripe_sectors,
            raid_save.region_sectors,
        )
    }

    pub fn create_redundant_zones(
        &mut self,
        dm: &DM,
        name: &str,
        block_devs: &BlockDevs,
    ) -> FroyoResult<bool> {
        let mut new_zones = false;
        while let Some(rd) = self.create_redundant_zone(&dm, name, &block_devs)? {
            self.raids.insert(rd.id.clone(), Rc::new(RefCell::new(rd)));
            new_zones = true;
        }

        Ok(new_zones)
    }

    // Try to make an as-large-as-possible redundant device from the
    // given block devices.
    fn create_redundant_zone(
        &mut self,
        dm: &DM,
        name: &str,
        block_devs: &BlockDevs,
    ) -> FroyoResult<Option<RaidDev>> {
        let scratch_needed = self.scratch_needed();

        // get common data area size, allowing for Froyo data at start and end
        let mut bd_areas: Vec<_> = block_devs
            .0
            .values()
            .filter_map(|bd| bd.present())
            .filter_map(|bd| match bd.borrow().largest_avail_area() {
                Some(x) => Some((bd.clone(), x.0, x.1)),
                None => None,
            })
            .filter(|&(_, _, len)| len >= scratch_needed)
            .map(|(bd, off, len)| (bd, off, len - scratch_needed))
            .filter(|&(_, _, len)| len >= MIN_DATA_ZONE_SECTORS)
            .collect();

        // Not enough devs with room for a raid device
        if bd_areas.len() < 2 {
            return Ok(None);
        }

        // Ensure we leave enough scratch space to handle a reshape
        let common_avail_sectors = bd_areas.iter().map(|&(_, _, len)| len).min().unwrap();

        // Absolute limit on each RAID size.
        let common_avail_sectors = min(common_avail_sectors, MAX_DATA_ZONE_SECTORS);

        // Also limit size in order to try to create a certain base
        // number of RAIDs, for reshape shenanigans.
        // Use size of 2nd largest bdev, which is guaranteed to be
        // used fully by raids, unlike the largest.
        let second_largest_bdev = {
            let mut sizes = block_devs
                .0
                .values()
                .filter_map(|bm| bm.present())
                .map(|bd| bd.borrow().sectors)
                .collect::<Vec<_>>();
            sizes.sort();
            sizes.pop();
            sizes.pop().unwrap()
        };
        let clamped_size = max(
            second_largest_bdev / Sectors(IDEAL_RAID_COUNT as u64),
            MIN_DATA_ZONE_SECTORS,
        );
        let common_avail_sectors = min(common_avail_sectors, clamped_size);

        // Handle raid regions and calc metadata size
        let (region_count, region_sectors) = {
            let mut region_sectors = DEFAULT_REGION_SECTORS;
            while *common_avail_sectors / *region_sectors > MAX_REGIONS {
                region_sectors = Sectors(*region_sectors * 2);
            }

            let partial_region = if common_avail_sectors % region_sectors == Sectors(0) {
                Sectors(0)
            } else {
                Sectors(1)
            };

            (
                common_avail_sectors / region_sectors + partial_region,
                region_sectors,
            )
        };

        // each region needs 1 bit in the write intent bitmap
        let mdata_sectors = Sectors(
            align_to(8192 + (*region_count / 8), SECTOR_SIZE).next_power_of_two() / SECTOR_SIZE,
        );
        // data size must be multiple of stripe size
        let data_sectors = (common_avail_sectors - mdata_sectors) & Sectors(!(*STRIPE_SECTORS - 1));

        let raid_uuid = Uuid::new_v4().to_simple_string();

        let mut linear_devs = Vec::new();
        for (num, &mut (ref mut bd, sector_start, _)) in bd_areas.iter_mut().enumerate() {
            let mdata_sector_start = sector_start;
            let data_sector_start = SectorOffset(*mdata_sector_start + *mdata_sectors);

            let linear = Rc::new(RefCell::new(LinearDev::new(
                &dm,
                &format!("{}-{}-{}", name, raid_uuid, num),
                bd,
                &[LinearSegment::new(mdata_sector_start, mdata_sectors)],
                &[LinearSegment::new(data_sector_start, data_sectors)],
            )?));

            bd.borrow_mut()
                .linear_devs
                .insert(linear.borrow().meta_dev.dm_name.clone(), linear.clone());

            linear_devs.push(RaidMember::Present(linear));
        }

        let raid = RaidDev::setup(
            &dm,
            &name,
            raid_uuid,
            linear_devs,
            STRIPE_SECTORS,
            region_sectors,
        )?;

        Ok(Some(raid))
    }

    pub fn alloc_raid_segments(&self, sectors: Sectors) -> Option<Vec<RaidSegment>> {
        let mut needed = sectors;
        let mut segs = Vec::new();
        for rd in self.raids.values() {
            if needed == Sectors(0) {
                break;
            }
            if !rd.borrow().is_safe() {
                continue;
            }
            let (gotten, r_segs) = rd.borrow().get_some_space(needed);
            segs.extend(
                r_segs
                    .iter()
                    .map(|&(start, len)| RaidSegment::new(start, len, RaidLayer::Raid(rd.clone()))),
            );
            needed = needed - gotten;
        }

        match *needed {
            0 => Some(segs),
            _ => None,
        }
    }

    pub fn lookup_segment(
        &self,
        id: &str,
        start: SectorOffset,
        length: Sectors,
    ) -> Option<RaidSegment> {
        match self.raids.get(id) {
            Some(rd) => Some(RaidSegment::new(start, length, RaidLayer::Raid(rd.clone()))),
            None => {
                // Before we give up, check the tempdev.
                if let Some(ref tempdev) = self.temp_dev {
                    if tempdev.borrow().id == id {
                        return Some(RaidSegment::new(
                            start,
                            length,
                            RaidLayer::Temp(tempdev.clone()),
                        ));
                    }
                }
                None
            }
        }
    }

    // We need scratch space on each drive for half the largest
    // raiddev capacity. This is overly generous but let's just do
    // this until we have reshape support
    fn scratch_needed(&self) -> Sectors {
        self.raids
            .values()
            .map(|rd| rd.borrow().length)
            .max()
            .unwrap_or_else(|| Sectors(0))
            / Sectors(2)
            + Sectors(1)
    }

    pub fn are_idle(&self) -> bool {
        self.raids
            .iter()
            .map(|(_, rd)| match rd.borrow().status() {
                Err(_) => false,
                Ok((_, action)) => match action {
                    RaidAction::Idle => true,
                    _ => false,
                },
            })
            .all(|res| res)
    }

    pub fn max_used_raid_sectors(&self) -> Sectors {
        self.raids
            .iter()
            .map(|(_, rd)| {
                rd.borrow()
                    .used_areas()
                    .into_iter()
                    .map(|(_, len)| len)
                    .sum_sectors()
            })
            .max()
            .unwrap_or_else(|| Sectors(0))
    }

    pub fn avail_space(&self) -> Sectors {
        self.raids
            .values()
            .map(|rd| rd.borrow().avail_sectors())
            .sum_sectors()
    }

    pub fn total_space(&self) -> Sectors {
        self.raids
            .values()
            .map(|rd| rd.borrow().length)
            .sum_sectors()
    }

    pub fn add_new_block_device(
        &mut self,
        froyo_id: &str,
        blockdev: &Rc<RefCell<BlockDev>>,
    ) -> FroyoResult<()> {
        let dm = DM::new()?;

        // let existing raids know about the new disk, maybe they're degraded
        for (_, raid) in &mut self.raids {
            raid.borrow_mut()
                .new_block_device_added(&dm, froyo_id, &blockdev)?;
        }

        Ok(())
    }

    pub fn add_existing_block_device(
        &mut self,
        froyo_id: &str,
        blockdev: &Rc<RefCell<BlockDev>>,
    ) -> FroyoResult<()> {
        let dm = DM::new()?;

        for (_, raid) in &mut self.raids {
            raid.borrow_mut()
                .block_device_found(&dm, froyo_id, &blockdev)?;
        }

        Ok(())
    }

    pub fn teardown(&self, dm: &DM) -> FroyoResult<()> {
        for raid in &mut self.raids.values() {
            raid.borrow_mut().teardown(&dm)?
        }

        Ok(())
    }
}

// A RaidSegment will almost always be on a RaidDev, unless we're
// reshaping and we had to temporarily copy it to a temp area.
#[derive(Debug, Clone)]
pub enum RaidLayer {
    Temp(Rc<RefCell<TempDev>>),
    Raid(Rc<RefCell<RaidDev>>),
}

impl RaidLayer {
    pub fn dstr(&self) -> String {
        match *self {
            RaidLayer::Temp(ref td) => td.borrow().dstr(),
            RaidLayer::Raid(ref rd) => rd.borrow().dev.dstr(),
        }
    }

    pub fn on_temp(&self) -> bool {
        if let RaidLayer::Temp(_) = *self {
            true
        } else {
            false
        }
    }

    pub fn raid(&self) -> Rc<RefCell<RaidDev>> {
        match *self {
            RaidLayer::Raid(ref rd) => rd.clone(),
            _ => panic!("should never happen"),
        }
    }

    pub fn id(&self) -> String {
        match *self {
            RaidLayer::Temp(ref td) => td.borrow().id.to_owned(),
            RaidLayer::Raid(ref rd) => rd.borrow().id.to_owned(),
        }
    }
}

// Not Clone, b/c that would mess up our parent.used stuff
pub struct RaidSegment {
    pub start: SectorOffset,
    pub length: Sectors,
    pub parent: RaidLayer,
}

// The derived Debug ends up recursing, so do this instead
impl fmt::Debug for RaidSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.parent {
            RaidLayer::Temp(ref p) => write!(
                f,
                "(temp {}, {}, {})",
                *self.start,
                *self.length,
                p.borrow().dstr()
            ),
            RaidLayer::Raid(ref p) => write!(
                f,
                "(raid {}, {}, {})",
                *self.start,
                *self.length,
                p.borrow().id
            ),
        }
    }
}

impl RaidSegment {
    pub fn new(start: SectorOffset, length: Sectors, parent: RaidLayer) -> RaidSegment {
        if let RaidLayer::Raid(ref rd) = parent {
            rd.borrow_mut().used.insert(start, length);
        }
        RaidSegment {
            start: start,
            length: length,
            parent: parent,
        }
    }

    // Also update the used map over in self.parent.used.
    pub fn update_length(&mut self, length: Sectors) {
        let rd = self.parent.raid();
        let mut parent = rd.borrow_mut();
        let entry = parent.used.get_mut(&self.start).unwrap();
        *entry = length;
        self.length = length;
    }

    pub fn to_save(&self) -> RaidSegmentSave {
        RaidSegmentSave {
            start: self.start,
            length: self.length,
            parent: self.parent.id(),
        }
    }
}

impl Drop for RaidSegment {
    fn drop(&mut self) {
        match self.parent {
            RaidLayer::Raid(ref rd) => {
                rd.borrow_mut().used.remove(&self.start);
            }
            RaidLayer::Temp(_) => {}
        };
    }
}

#[derive(Debug)]
pub struct RaidLinearDev {
    id: String,
    pub dev: DmDevice,
    pub segments: Vec<RaidSegment>,
}

impl RaidLinearDev {
    pub fn dm_table(segments: &[RaidSegment]) -> Vec<(u64, u64, String, String)> {
        let mut table = Vec::new();
        let mut offset = SectorOffset(0);
        for seg in segments {
            let dstr = seg.parent.dstr();
            let line = (
                *offset,
                *seg.length,
                "linear".to_owned(),
                format!("{} {}", dstr, *seg.start),
            );
            table.push(line);
            offset = offset + SectorOffset(*seg.length);
        }

        table
    }

    pub fn new(
        dm: &DM,
        name: &str,
        id: &str,
        segments: Vec<RaidSegment>,
    ) -> FroyoResult<RaidLinearDev> {
        Self::setup(dm, name, id, segments)
    }

    pub fn setup(
        dm: &DM,
        name: &str,
        id: &str,
        segments: Vec<RaidSegment>,
    ) -> FroyoResult<RaidLinearDev> {
        let table = Self::dm_table(&segments);
        let dm_name = format!("froyo-raid-linear-{}", name);
        let linear_dev = DmDevice::new(dm, &dm_name, &table)?;

        Ok(RaidLinearDev {
            id: id.to_owned(),
            dev: linear_dev,
            segments: segments,
        })
    }

    pub fn teardown(&mut self, dm: &DM) -> FroyoResult<()> {
        self.dev.teardown(dm)?;

        Ok(())
    }

    pub fn to_save(&self) -> RaidLinearDevSave {
        RaidLinearDevSave {
            id: self.id.clone(),
            segments: self.segments.iter().map(|x| x.to_save()).collect(),
        }
    }

    pub fn length(&self) -> Sectors {
        self.segments.iter().map(|x| x.length).sum_sectors()
    }

    pub fn extend(&mut self, segs: Vec<RaidSegment>) -> FroyoResult<()> {
        // last existing and first new may be contiguous
        let coalesced_new_first = {
            let old_last = self.segments.last_mut().unwrap();
            let new_first = segs.first().unwrap();
            if old_last.parent.id() == new_first.parent.id()
                && (old_last.start + SectorOffset(*old_last.length) == new_first.start)
            {
                let new_len = old_last.length + new_first.length;
                old_last.update_length(new_len);
                true
            } else {
                false
            }
        };

        if coalesced_new_first {
            self.segments.extend(segs.into_iter().skip(1));
        } else {
            self.segments.extend(segs);
        }

        let table = RaidLinearDev::dm_table(&self.segments);

        let dm = DM::new()?;
        self.dev.reload(&dm, &table)?;

        Ok(())
    }

    pub fn parents(&self) -> BTreeMap<String, Rc<RefCell<RaidDev>>> {
        let mut map = BTreeMap::new();

        for rs in &self.segments {
            if !rs.parent.on_temp() {
                map.insert(rs.parent.id(), rs.parent.raid().clone());
            }
        }

        map
    }
}
