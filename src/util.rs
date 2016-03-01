// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fs::{File, OpenOptions};
use std::io;
use std::io::{Write, ErrorKind};
use std::borrow::Borrow;
use std::os::unix::prelude::AsRawFd;

use devicemapper::{DM, Device, DmFlags, DevId};

use consts::*;
use types::{FroyoResult, FroyoError};

pub fn align_to(num: u64, align_to: u64) -> u64 {
    let agn = align_to - 1;

    (num + agn) & !agn
}

ioctl!(read blkgetsize64 with 0x12, 114; u64);

pub fn blkdev_size(file: &File) -> FroyoResult<u64> {
    let mut val: u64 = 0;

    match unsafe { blkgetsize64(file.as_raw_fd(), &mut val) } {
        Err(x) => Err(FroyoError::Nix(x)),
        Ok(_) => Ok(val),
    }
}

pub fn setup_dm_dev<T1, T2>(dm: &DM, name: &str, targets: &[(u64, u64, T1, T2)])
                            -> FroyoResult<Device>
    where T1: Borrow<str>,
          T2: Borrow<str>,
{
    let id = &DevId::Name(name);

    if let Ok(di) = dm.device_status(id) {
        dbgp!("Found {}", name);
        return Ok(di.device())
    }

    try!(dm.device_create(&name, None, DmFlags::empty()));
    let di = try!(dm.table_load(id, &targets));
    try!(dm.device_suspend(id, DmFlags::empty()));

    dbgp!("Created {}", name);

    Ok(di.device())
}

pub fn clear_dev(dev: Device) -> FroyoResult<()> {
    let pathbuf = dev.path().unwrap();

    let mut f = match OpenOptions::new().write(true).open(&pathbuf) {
        Err(_) => return Err(FroyoError::Io(io::Error::new(
            ErrorKind::PermissionDenied,
            format!("Could not open {}", pathbuf.display())))),
        Ok(x) => x,
    };

    let sectors = try!(blkdev_size(&f)) / SECTOR_SIZE;
    let buf = vec![0u8; SECTOR_SIZE as usize];
    for _ in 0..sectors {
        try!(f.write(&buf));
    }

    Ok(())
}
