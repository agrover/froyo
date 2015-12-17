// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fs::{File, OpenOptions};
use std::io;
use std::io::{Write, ErrorKind};
use std::borrow::Borrow;
use std::os::unix::prelude::AsRawFd;

use devicemapper::{DM, Device, DmFlags};
use nix::sys::ioctl;

use consts::*;

pub fn align_to(num: u64, align_to: u64) -> u64 {
    let agn = align_to - 1;

    (num + agn) & !agn
}

pub fn blkdev_size(file: &File) -> io::Result<u64> {
    // BLKGETSIZE64
    let op = ioctl::op_read(0x12, 114, 8);
    let mut val: u64 = 0;

    match unsafe { ioctl::read_into(file.as_raw_fd(), op, &mut val) } {
        Err(_) => return Err((io::Error::last_os_error())),
        Ok(_) => Ok(val),
    }
}

pub fn setup_dm_dev<T1, T2>(dm: &DM, name: &str, targets: &[(u64, u64, T1, T2)])
                    -> io::Result<Device>
        where T1: Borrow<str>,
              T2: Borrow<str>,
{
    if let Ok(di) = dm.device_status(name) {
        dbgp!("Found {}", name);
        return Ok(di.device())
    }

    try!(dm.device_create(&name, None, DmFlags::empty()));
    let di = try!(dm.table_load(&name, &targets));
    try!(dm.device_suspend(&name, DmFlags::empty()));

    dbgp!("Created {}", name);

    Ok(di.device())
}

pub fn clear_dev(dev: &Device) -> io::Result<()> {
    let pathbuf = dev.path().unwrap();

    let mut f = match OpenOptions::new().write(true).open(&pathbuf) {
        Err(_) => return Err(io::Error::new(
            ErrorKind::PermissionDenied,
            format!("Could not open {}", pathbuf.display()))),
        Ok(x) => x,
    };

    let sectors = try!(blkdev_size(&f)) / SECTOR_SIZE;
    let buf = vec![0u8; SECTOR_SIZE as usize];
    for _ in 0..sectors {
        try!(f.write(&buf));
    }

    Ok(())
}

