// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::io;
use std::error::Error;
use std::process::Command;
use std::fs;
use std::path::PathBuf;

use devicemapper::{DM, Device, DmFlags, DevId};
use uuid::Uuid;
use nix::sys::stat::{mknod, umask, Mode, S_IFBLK, S_IRUSR, S_IWUSR, S_IRGRP, S_IWGRP};

use types::{Sectors, DataBlocks, FroyoError};
use raid::{RaidDev, RaidSegment, RaidLinearDev, RaidLinearDevSave};
use util::{clear_dev, setup_dm_dev};
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
    low_water_blocks: DataBlocks,
    meta_dev: RaidLinearDev,
    data_dev: RaidLinearDev,
}

#[derive(Debug, Clone, Copy)]
pub struct ThinPoolBlockUsage {
    pub used_meta: u64,
    pub total_meta: u64,
    pub used_data: DataBlocks,
    pub total_data: DataBlocks,
}

pub enum ThinPoolStatus {
    Good((ThinPoolWorkingStatus, ThinPoolBlockUsage)),
    Fail,
}

pub enum ThinPoolWorkingStatus {
    Good,
    ReadOnly,
    OutOfSpace,
    NeedsCheck,
}

impl ThinPoolDev {
    fn get_raid_segments(sectors: Sectors, devs: &BTreeMap<String, Rc<RefCell<RaidDev>>>)
                         -> Option<Vec<Rc<RefCell<RaidSegment>>>> {
        let mut needed = sectors;
        let mut segs = Vec::new();
        for (_, rd) in devs {
            if needed == Sectors::new(0) {
                break
            }
            let (gotten, r_segs) = RefCell::borrow(rd).get_some_space(sectors);
            segs.extend(r_segs.iter()
                        .map(|&(start, len)| RaidSegment::new(start, len, rd)));
            needed = needed - gotten;
        }

        match *needed {
            0 => Some(segs),
            _ => None,
        }
    }

    pub fn new(dm: &DM, name: &str, devs: &BTreeMap<String, Rc<RefCell<RaidDev>>>)
               -> io::Result<ThinPoolDev> {

        let meta_size = Sectors::new(8192);
        let data_size = Sectors::new(2 * 2 * 1024 * 1024);

        let meta_raid_segments = try!(ThinPoolDev::get_raid_segments(meta_size, devs).ok_or(
            io::Error::new(io::ErrorKind::InvalidInput,
                           "no space for thinpool meta")));
        let data_raid_segments = try!(ThinPoolDev::get_raid_segments(data_size, devs).ok_or(
            io::Error::new(io::ErrorKind::InvalidInput,
                           "no space for thinpool data")));

        // meta
        let meta_name = format!("thin-meta-{}", name);
        let meta_raid_dev = try!(RaidLinearDev::create(
            dm,
            &meta_name,
            &Uuid::new_v4().to_simple_string(),
            meta_raid_segments));

        try!(clear_dev(&meta_raid_dev.dev));

        // data
        let data_name = format!("thin-data-{}", name);
        let data_raid_dev = try!(RaidLinearDev::create(
            dm,
            &data_name,
            &Uuid::new_v4().to_simple_string(),
            data_raid_segments));

        let data_block_size = Sectors::new(2048); // 1MiB
        let low_water_blocks = DataBlocks::new(512); // 512MiB

        ThinPoolDev::create(
            dm,
            name,
            data_block_size,
            low_water_blocks,
            meta_raid_dev,
            data_raid_dev)
    }

    pub fn create(
        dm: &DM,
        name: &str,
        data_block_size: Sectors,
        low_water_blocks: DataBlocks,
        meta_raid_dev: RaidLinearDev,
        data_raid_dev: RaidLinearDev)
        -> io::Result<ThinPoolDev> {

        let params = format!("{}:{} {}:{} {} {}",
                             meta_raid_dev.dev.major,
                             meta_raid_dev.dev.minor,
                             data_raid_dev.dev.major,
                             data_raid_dev.dev.minor,
                             *data_block_size,
                             *low_water_blocks);
        let table = [(0u64, *data_raid_dev.length(), "thin-pool", params)];

        let dm_name = format!("froyo-thin-pool-{}", name);
        let pool_dev = try!(setup_dm_dev(dm, &dm_name, &table));

        Ok(ThinPoolDev {
            dm_name: dm_name,
            dev: pool_dev,
            data_block_size: data_block_size,
            low_water_blocks: low_water_blocks,
            meta_dev: meta_raid_dev,
            data_dev: data_raid_dev,
        })
    }

    pub fn to_save(&self) -> ThinPoolDevSave {
        ThinPoolDevSave {
            data_block_size: self.data_block_size,
            low_water_blocks: self.low_water_blocks,
            meta_dev: self.meta_dev.to_save(),
            data_dev: self.data_dev.to_save(),
        }
    }

    pub fn status(&self) -> Result<ThinPoolStatus, FroyoError> {
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
            "rw" => return Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::Good, usage))),
            "ro" => return Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::ReadOnly, usage))),
            "out_of_data_space" => return Ok(ThinPoolStatus::Good(
                (ThinPoolWorkingStatus::OutOfSpace, usage))),
            _ => return Err(FroyoError::Io(io::Error::new(
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

}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThinDevSave {
    pub thin_number: u32,
    pub fs: String,
    pub size: Sectors,
}

#[derive(Debug, Clone)]
pub struct ThinDev {
    dev: Device,
    thin_number: u32,
    fs: String,
    pub size: Sectors,
    dm_name: String,
}

pub enum ThinStatus {
    Good(Sectors),
    Fail,
}

impl ThinDev {
    pub fn create(
        dm: &DM,
        name: &str,
        thin_number: u32,
        fs: &str,
        size: Sectors,
        pool_dev: &ThinPoolDev)
        -> io::Result<ThinDev> {
        match dm.target_msg(&DevId::Name(&pool_dev.dm_name),
                            0, &format!("create_thin {}", thin_number)) {
            Err(x) => dbgp!("create_thin message failed: {}", x.description()),
            Ok(_) => {},
        }

        let params = format!("{}:{} {}", pool_dev.dev.major, pool_dev.dev.minor, thin_number);
        let table = [(0u64, *size, "thin", params)];

        let dm_name = format!("froyo-thin-{}-{}", name, thin_number);
        let thin_dev = try!(setup_dm_dev(dm, &dm_name, &table));

        Ok(ThinDev {
            dev: thin_dev,
            thin_number: thin_number,
            fs: fs.to_owned(),
            size: size,
            dm_name: dm_name,
        })
    }

    pub fn to_save(&self) -> ThinDevSave {
        ThinDevSave {
            thin_number: self.thin_number,
            fs: self.fs.clone(),
            size: self.size,
        }
    }

    pub fn status(&self) -> Result<ThinStatus, FroyoError> {
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

        Ok(ThinStatus::Good(Sectors::new(status_vals[0].parse::<u64>().unwrap())))
    }

    pub fn create_devnode(&mut self, name: &str) -> Result<(), FroyoError> {
        let mut pathbuf = PathBuf::from("/dev/froyo");

        match fs::create_dir(&pathbuf) {
            Ok(()) => {},
            Err(e) => {
                match e.kind() {
                    io::ErrorKind::AlreadyExists => {},
                    _ => return Err(FroyoError::Io(e)),
                }
            }
        }

        pathbuf.push(name);

        let old_umask = umask(Mode::empty());
        try!(mknod(&pathbuf, S_IFBLK, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP, self.dev.into()));
        umask(old_umask);

        Ok(())
    }

    pub fn create_fs(&mut self, name: &str) -> Result<(), FroyoError> {
        let dev_name = format!("/dev/froyo/{}", name);
        let output = try!(Command::new("mkfs.xfs")
                          .arg("-f")
                          .arg(&dev_name)
                          .output());

        if output.status.success(){
            dbgp!("Created xfs filesystem on {}", dev_name)
        } else {
            println!("err {}", String::from_utf8_lossy(&output.stderr));
        }
        Ok(())
    }
}
