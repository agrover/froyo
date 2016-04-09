// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;
use std::process::Command;
use std::fs;
use std::path::PathBuf;

use devicemapper::{DM, Device, DmFlags, DevId, DM_SUSPEND};
use uuid::Uuid;
use nix::sys::stat::{mknod, umask, Mode, S_IFBLK, S_IRUSR, S_IWUSR, S_IRGRP, S_IWGRP};
use nix::errno::EEXIST;

use types::{Sectors, DataBlocks, FroyoError, FroyoResult, InternalError};
use raid::{RaidSegment, RaidLinearDev, RaidLinearDevSave};
use util::{clear_dev, setup_dm_dev, teardown_dm_dev};
use consts::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThinPoolDevSave {
    pub data_block_size: Sectors,
    pub low_water_blocks: DataBlocks,
    pub meta_dev: RaidLinearDevSave,
    pub data_dev: RaidLinearDevSave,
}

#[derive(Debug, Clone)]
pub struct ThinPoolDev {
    dm_name: String,
    dev: Device,
    data_block_size: Sectors,
    pub low_water_blocks: DataBlocks,
    params: String,
    pub meta_dev: RaidLinearDev,
    pub data_dev: RaidLinearDev,
}

#[derive(Debug, Clone, Copy)]
pub struct ThinPoolBlockUsage {
    pub used_meta: u64,
    pub total_meta: u64,
    pub used_data: DataBlocks,
    pub total_data: DataBlocks,
}

#[derive(Debug, Clone, Copy)]
pub enum ThinPoolStatus {
    Good((ThinPoolWorkingStatus, ThinPoolBlockUsage)),
    Fail,
}

#[derive(Debug, Clone, Copy)]
pub enum ThinPoolWorkingStatus {
    Good,
    ReadOnly,
    OutOfSpace,
    NeedsCheck,
}

impl ThinPoolDev {
    pub fn new(dm: &DM,
               id: &str,
               meta_segs: Vec<RaidSegment>,
               data_segs: Vec<RaidSegment>)
               -> FroyoResult<ThinPoolDev> {
        // meta
        let meta_name = format!("thin-meta-{}", id);
        let meta_raid_dev = try!(RaidLinearDev::new(
            dm,
            &meta_name,
            &Uuid::new_v4().to_simple_string(),
            meta_segs));

        try!(clear_dev(meta_raid_dev.dev));

        // data
        let data_name = format!("thin-data-{}", id);
        let data_raid_dev = try!(RaidLinearDev::new(
            dm,
            &data_name,
            &Uuid::new_v4().to_simple_string(),
            data_segs));

        ThinPoolDev::setup(
            dm,
            id,
            DATA_BLOCK_SIZE,
            DataBlocks::new(TPOOL_LOW_WATER_BLOCKS),
            meta_raid_dev,
            data_raid_dev)
    }

    pub fn setup(
        dm: &DM,
        id: &str,
        data_block_size: Sectors,
        low_water_blocks: DataBlocks,
        meta_raid_dev: RaidLinearDev,
        data_raid_dev: RaidLinearDev)
        -> FroyoResult<ThinPoolDev> {

        let params = format!("{}:{} {}:{} {} {}",
                             meta_raid_dev.dev.major,
                             meta_raid_dev.dev.minor,
                             data_raid_dev.dev.major,
                             data_raid_dev.dev.minor,
                             *data_block_size,
                             *low_water_blocks);
        let table = [(0u64, *data_raid_dev.length(), "thin-pool", &*params)];

        let dm_name = format!("froyo-thin-pool-{}", id);
        let pool_dev = try!(setup_dm_dev(dm, &dm_name, &table));

        Ok(ThinPoolDev {
            dm_name: dm_name,
            dev: pool_dev,
            data_block_size: data_block_size,
            low_water_blocks: low_water_blocks,
            params: params.clone(),
            meta_dev: meta_raid_dev,
            data_dev: data_raid_dev,
        })
    }

    pub fn teardown(&mut self, dm: &DM) -> FroyoResult<()> {
        try!(teardown_dm_dev(dm, &self.dm_name));
        try!(self.meta_dev.teardown(dm));
        try!(self.data_dev.teardown(dm));

        Ok(())
    }

    pub fn to_save(&self) -> ThinPoolDevSave {
        ThinPoolDevSave {
            data_block_size: self.data_block_size,
            low_water_blocks: self.low_water_blocks,
            meta_dev: self.meta_dev.to_save(),
            data_dev: self.data_dev.to_save(),
        }
    }

    pub fn status(&self) -> FroyoResult<ThinPoolStatus> {
        let dm = try!(DM::new());

        let (_, mut status) = try!(
            dm.table_status(&DevId::Name(&self.dm_name), DmFlags::empty()));

        if status.len() != 1 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Expected 1 line from thin pool status")))
        }

        let status_line = status.pop().unwrap().3;
        if status_line.starts_with("Fail") {
            return Ok(ThinPoolStatus::Fail)
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        if status_vals.len() < 8 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Kernel returned too few values from thin pool status")))
        }

        let usage = {
            let meta_vals = status_vals[1].split('/').collect::<Vec<_>>();
            let data_vals = status_vals[2].split('/').collect::<Vec<_>>();
            ThinPoolBlockUsage {
                used_meta: meta_vals[0].parse::<u64>().unwrap(),
                total_meta: meta_vals[1].parse::<u64>().unwrap(),
                used_data: DataBlocks::new(data_vals[0].parse::<u64>().unwrap()),
                total_data: DataBlocks::new(data_vals[1].parse::<u64>().unwrap()),
            }
        };

        match status_vals[7] {
            "-" => {},
            "needs_check" => return Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::NeedsCheck, usage))),
            _ => return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Kernel returned unexpected value in thin pool status")))
        }

        match status_vals[4] {
            "rw" => Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::Good, usage))),
            "ro" => Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::ReadOnly, usage))),
            "out_of_data_space" => Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::OutOfSpace, usage))),
            _ => Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Kernel returned unexpected value in thin pool status")))
        }
    }

    // return size of a data block in bytes
    pub fn data_block_size(&self) -> u64 {
        *self.data_block_size * SECTOR_SIZE
    }

    pub fn sectors_to_blocks(&self, sectors: Sectors) -> DataBlocks {
        DataBlocks::new(*sectors / *self.data_block_size)
    }

    pub fn blocks_to_sectors(&self, blocks: DataBlocks) -> Sectors {
        Sectors::new(*blocks * *self.data_block_size)
    }

    pub fn extend_data_dev(&mut self, segs: Vec<RaidSegment>)
                           -> FroyoResult<()> {
        try!(self.data_dev.extend(segs));
        try!(self.dm_reload());
        Ok(())
    }

    pub fn extend_meta_dev(&mut self, segs: Vec<RaidSegment>)
                           -> FroyoResult<()> {
        try!(self.meta_dev.extend(segs));
        try!(self.dm_reload());
        Ok(())
    }

    fn dm_reload(&self) -> FroyoResult<()> {
        let dm = try!(DM::new());
        let id = &DevId::Name(&self.dm_name);

        let table = [(0u64, *self.data_dev.length(), "thin-pool", &*self.params)];

        try!(dm.table_load(id, &table));
        try!(dm.device_suspend(id, DM_SUSPEND));
        try!(dm.device_suspend(id, DmFlags::empty()));
        Ok(())
    }

    pub fn used_sectors(&self) -> Sectors {
        self.meta_dev.length() + self.data_dev.length()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThinDevSave {
    pub name: String,
    pub thin_number: u32,
    pub size: Sectors,
}

#[derive(Debug, Clone)]
pub struct ThinDev {
    dev: Device,
    name: String,
    pub thin_number: u32,
    pub size: Sectors,
    dm_name: String,
    params: String,
}

#[derive(Debug, Clone, Copy)]
pub enum ThinStatus {
    Good(Sectors),
    Fail,
}

impl ThinDev {
    pub fn new(
        dm: &DM,
        froyo_id: &str,
        name: &str,
        thin_number: u32,
        size: Sectors,
        pool_dev: &ThinPoolDev)
        -> FroyoResult<ThinDev> {

        try!(dm.target_msg(&DevId::Name(&pool_dev.dm_name),
                           0, &format!("create_thin {}", thin_number)));

        let mut td = try!(ThinDev::setup(
            dm,
            froyo_id,
            name,
            thin_number,
            size,
            pool_dev));

        try!(td.create_fs(name));

        Ok(td)
    }

    pub fn setup(
        dm: &DM,
        froyo_id: &str,
        name: &str,
        thin_number: u32,
        size: Sectors,
        pool_dev: &ThinPoolDev)
        -> FroyoResult<ThinDev> {

        let params = format!("{}:{} {}", pool_dev.dev.major,
                             pool_dev.dev.minor, thin_number);
        let table = [(0u64, *size, "thin", &*params)];

        let dm_name = format!("froyo-thin-{}-{}", froyo_id, thin_number);
        let thin_dev = try!(setup_dm_dev(dm, &dm_name, &table));

        try!(ThinDev::create_devnode(name, thin_dev));

        Ok(ThinDev {
            dev: thin_dev,
            name: name.to_owned(),
            thin_number: thin_number,
            size: size,
            dm_name: dm_name,
            params: params.clone(),
        })
    }

    pub fn teardown(&mut self, dm: &DM) -> FroyoResult<()> {
        try!(self.remove_devnode());

        try!(teardown_dm_dev(dm, &self.dm_name));

        Ok(())
    }

    pub fn extend(&mut self, sectors: Sectors) -> FroyoResult<()> {

        self.size = self.size + sectors;

        let dm = try!(DM::new());
        let id = &DevId::Name(&self.dm_name);

        let table = [(0u64, *self.size, "thin", &*self.params)];

        try!(dm.table_load(id, &table));
        try!(dm.device_suspend(id, DM_SUSPEND));
        try!(dm.device_suspend(id, DmFlags::empty()));

        // TODO: we need to know where it's mounted in order to call
        // this
        // let output = try!(Command::new("xfs_growfs")
        //                   .arg(&mount_point)
        //                   .output());

        Ok(())
    }

    pub fn to_save(&self) -> ThinDevSave {
        ThinDevSave {
            name: self.name.clone(),
            thin_number: self.thin_number,
            size: self.size,
        }
    }

    pub fn status(&self) -> FroyoResult<ThinStatus> {
        let dm = try!(DM::new());

        let (_, mut status) = try!(
            dm.table_status(&DevId::Name(&self.dm_name), DmFlags::empty()));

        if status.len() != 1 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Expected 1 line from thin status")))
        }

        // We should either get 1 line or the kernel is broken
        let status_line = status.pop().unwrap().3;
        if status_line.starts_with("Fail") {
            return Ok(ThinStatus::Fail)
        }
        let status_vals = status_line.split(' ').collect::<Vec<_>>();

        Ok(ThinStatus::Good(Sectors::new(
            status_vals[0].parse::<u64>().unwrap())))
    }

    fn create_devnode(name: &str, dev: Device) -> FroyoResult<()> {
        let mut pathbuf = PathBuf::from("/dev/froyo");

        if let Err(e) = fs::create_dir(&pathbuf) {
            if e.kind() != io::ErrorKind::AlreadyExists {
                return Err(FroyoError::Io(e))
            }
        }

        pathbuf.push(name);

        let old_umask = umask(Mode::empty());
        let res = mknod(&pathbuf,
                    S_IFBLK,
                    S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP,
                    dev.into());
        umask(old_umask);
        if let Err(e) = res {
            if e.errno() != EEXIST {
                return Err(FroyoError::Nix(e))
            }
        }

        Ok(())
    }

    fn remove_devnode(&mut self) -> FroyoResult<()> {
        let mut pathbuf = PathBuf::from("/dev/froyo");
        pathbuf.push(&self.name);
        try!(fs::remove_file(&pathbuf));

        Ok(())
    }

    fn create_fs(&mut self, name: &str) -> FroyoResult<()> {
        let dev_name = format!("/dev/froyo/{}", name);
        let output = try!(Command::new("mkfs.xfs")
                          .arg("-f")
                          .arg(&dev_name)
                          .output());

        if output.status.success(){
            dbgp!("Created xfs filesystem on {}", dev_name)
        } else {
            return Err(FroyoError::Froyo(InternalError(
                format!("XFS mkfs error: {}",
                        String::from_utf8_lossy(&output.stderr)))))
        }
        Ok(())
    }
}
