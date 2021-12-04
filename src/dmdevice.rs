// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::borrow::Borrow;
use std::fs::OpenOptions;
use std::io;
use std::io::{ErrorKind, Write};

use devicemapper::dm::{DevId, DM};
use devicemapper::{consts::DmFlags, consts::DM_SUSPEND, types::TargetLine, Device};

use consts::*;
use types::{FroyoError, FroyoResult};
use util::blkdev_size;

#[derive(Debug, Clone, PartialEq)]
pub struct DmDevice {
    pub dm_name: String,
    pub dev: Device,
}

impl DmDevice {
    pub fn new<T1, T2>(dm: &DM, name: &str, table: &[(u64, u64, T1, T2)]) -> FroyoResult<DmDevice>
    where
        T1: Borrow<str>,
        T2: Borrow<str>,
    {
        let id = &DevId::Name(name);

        let di = match dm.device_status(id) {
            Ok(di) => {
                dbgp!("Found {}", name);
                di
            }
            Err(_) => {
                dm.device_create(&name, None, DmFlags::empty())?;
                let di = dm.table_load(id, table)?;
                dm.device_suspend(id, DmFlags::empty())?;

                dbgp!("Created {}", name);
                di
            }
        };

        Ok(DmDevice {
            dm_name: name.to_owned(),
            dev: di.device(),
        })
    }

    pub fn dstr(&self) -> String {
        format!("{}:{}", self.dev.major, self.dev.minor)
    }

    pub fn reload<T1, T2>(&self, dm: &DM, table: &[(u64, u64, T1, T2)]) -> FroyoResult<()>
    where
        T1: Borrow<str>,
        T2: Borrow<str>,
    {
        let id = &DevId::Name(&self.dm_name);

        dm.table_load(id, table)?;
        dm.device_suspend(id, DM_SUSPEND)?;
        dm.device_suspend(id, DmFlags::empty())?;

        Ok(())
    }

    pub fn suspend(&self, dm: &DM) -> FroyoResult<()> {
        dm.device_suspend(&DevId::Name(&self.dm_name), DM_SUSPEND)?;

        Ok(())
    }

    pub fn unsuspend(&self, dm: &DM) -> FroyoResult<()> {
        dm.device_suspend(&DevId::Name(&self.dm_name), DmFlags::empty())?;

        Ok(())
    }

    pub fn table_load<T1, T2>(&self, dm: &DM, table: &[(u64, u64, T1, T2)]) -> FroyoResult<()>
    where
        T1: Borrow<str>,
        T2: Borrow<str>,
    {
        dm.table_load(&DevId::Name(&self.dm_name), table)?;

        Ok(())
    }

    pub fn teardown(&self, dm: &DM) -> FroyoResult<()> {
        dbgp!("tearing down {}", self.dm_name);
        dm.device_remove(&DevId::Name(&self.dm_name), DmFlags::empty())?;

        Ok(())
    }

    pub fn clear(&self) -> FroyoResult<()> {
        let pathbuf = self.dev.devnode().unwrap();

        let mut f = match OpenOptions::new().write(true).open(&pathbuf) {
            Err(_) => {
                return Err(FroyoError::Io(io::Error::new(
                    ErrorKind::PermissionDenied,
                    format!("Could not open {}", pathbuf.display()),
                )))
            }
            Ok(x) => x,
        };

        let sectors = blkdev_size(&f)? / SECTOR_SIZE;
        let buf = vec![0u8; SECTOR_SIZE as usize];
        for _ in 0..sectors {
            f.write(&buf)?;
        }

        Ok(())
    }

    pub fn table_status(&self, dm: &DM) -> FroyoResult<Vec<TargetLine>> {
        let (_, status) = dm.table_status(&DevId::Name(&self.dm_name), DmFlags::empty())?;

        Ok(status)
    }

    pub fn message(&self, dm: &DM, message: &str) -> FroyoResult<()> {
        dm.target_msg(&DevId::Name(&self.dm_name), 0, message)?;

        Ok(())
    }
}
