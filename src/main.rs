// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//#![feature(slice_bytes, custom_derive, plugin, iter_cmp, iter_arith)]
#![feature(slice_bytes, iter_cmp, iter_arith, zero_one, custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate devicemapper;
#[macro_use]
extern crate clap;
extern crate nix;
extern crate crc;
extern crate byteorder;
extern crate uuid;
extern crate time;
extern crate serde;
extern crate serde_json;

#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;

#[allow(unused_imports)]
use std::io;
use std::io::{Read, Write, ErrorKind, Seek, SeekFrom};
use std::error::Error;
use std::process::exit;
use std::fs::{File, OpenOptions, read_dir};
use std::path::{Path, PathBuf};
use std::str::{FromStr, from_utf8};
use std::slice::bytes::copy_memory;
use std::os::unix::prelude::AsRawFd;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::num::Zero;

use devicemapper::{DM, Device, DmFlags};
use clap::{App, Arg, SubCommand, ArgMatches};
use nix::sys::{stat, ioctl};
use crc::crc32;
use byteorder::{LittleEndian, ByteOrder};
use uuid::Uuid;
use serde::ser::impls::SeqIteratorVisitor;

//
// Use distinct 'newtype' types for sectors and sector offsets for type safety.
// When needed, these can still be derefed to u64.
// Derive a bunch of stuff so we can do ops on them.
//
custom_derive! {
    #[derive(NewtypeFrom, NewtypeAdd, NewtypeSub, NewtypeDeref,
             NewtypeBitAnd, NewtypeNot, NewtypeDiv, NewtypeRem,
             Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    pub struct Sectors(u64);
}

impl Zero for Sectors {
    fn zero() -> Self {
        Sectors(0)
    }
}

impl serde::Serialize for Sectors {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::Serializer,
    {
        serializer.visit_u64(**self)
    }
}

custom_derive! {
    #[derive(NewtypeFrom, NewtypeAdd, NewtypeSub, NewtypeDeref,
             NewtypeBitAnd, NewtypeNot, NewtypeDiv, NewtypeRem,
             Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    pub struct SectorOffset(u64);
}

impl Zero for SectorOffset {
    fn zero() -> Self {
        SectorOffset(0)
    }
}

impl serde::Serialize for SectorOffset {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::Serializer,
    {
        serializer.visit_u64(**self)
    }
}

const SECTOR_SIZE: u64 = 512;
const HEADER_SIZE: u64 = 512;
const MDA_ZONE_SIZE: u64 = (1024 * 1024);
const MDA_ZONE_SECTORS: Sectors = Sectors(MDA_ZONE_SIZE / SECTOR_SIZE);
const FRO_MAGIC: &'static [u8] = b"!IamFroy0\x86\xffGO\x02^\x41";
const STRIPE_SECTORS: Sectors = Sectors(2048);

// No devs smaller than around a gig
const MIN_DATA_ZONE_SIZE: u64 = (1024 * 1024 * 1024);
const MIN_DATA_ZONE_SECTORS: Sectors = Sectors(MIN_DATA_ZONE_SIZE / SECTOR_SIZE);
const MIN_DEV_SIZE: u64 = MIN_DATA_ZONE_SIZE + (2 * MDA_ZONE_SIZE);
//const MIN_DEV_SECTORS: u64 = MIN_DEV_SIZE / SECTOR_SIZE;

const MAX_REGIONS: u64 = (2 * 1024 * 1024);
const DEFAULT_REGION_SIZE: u64 = (4 * 1024 * 1024);
const DEFAULT_REGION_SECTORS: Sectors = Sectors(DEFAULT_REGION_SIZE / SECTOR_SIZE);

static mut debug: bool = false;

macro_rules! dbgp {
    ($($arg:tt)*) => (
        unsafe {
            if debug {
                println!($($arg)*)
            }
        })
}

fn align_to(num: u64, align_to: u64) -> u64 {
    let agn = align_to - 1;

    (num + agn) & !agn
}

// Define a common error enum.
// See http://blog.burntsushi.net/rust-error-handling/
#[derive(Debug)]
pub enum FroyoError {
    Io(io::Error),
    Serde(serde_json::error::Error),
}

impl fmt::Display for FroyoError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FroyoError::Io(ref err) => write!(f, "IO error: {}", err),
            FroyoError::Serde(ref err) => write!(f, "Serde error: {}", err),
        }
    }
}

impl Error for FroyoError {
    fn description(&self) -> &str {
        match *self {
            FroyoError::Io(ref err) => err.description(),
            FroyoError::Serde(ref err) => Error::description(err),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            FroyoError::Io(ref err) => Some(err),
            FroyoError::Serde(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for FroyoError {
    fn from(err: io::Error) -> FroyoError {
        FroyoError::Io(err)
    }
}

impl From<serde_json::error::Error> for FroyoError {
    fn from(err: serde_json::error::Error) -> FroyoError {
        FroyoError::Serde(err)
    }
}


fn blkdev_size(file: &File) -> io::Result<u64> {
    // BLKGETSIZE64
    let op = ioctl::op_read(0x12, 114, 8);
    let mut val: u64 = 0;

    match unsafe { ioctl::read_into(file.as_raw_fd(), op, &mut val) } {
        Err(_) => return Err((io::Error::last_os_error())),
        Ok(_) => Ok(val),
    }
}

#[derive(Debug, Clone)]
pub struct BlockDev {
    dev: Device,
    id: String,
    path: PathBuf,
    sectors: Sectors,
    linear_devs: Vec<Rc<RefCell<LinearDev>>>,
}

impl serde::Serialize for BlockDev {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::Serializer
    {
        serializer.visit_struct("BlockDev", BlockDevVisitor {
            value: self,
            state: 0,
        })
    }
}

struct BlockDevVisitor<'a> {
    value: &'a BlockDev,
    state: u8,
}

impl<'a> serde::ser::MapVisitor for BlockDevVisitor<'a> {
    fn visit<S>(&mut self, serializer: &mut S) -> Result<Option<()>, S::Error>
        where S: serde::Serializer
    {
        match self.state {
            0 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("dev", &self.value.dev))))
            }
            1 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("id", &self.value.id))))
            }
            2 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("path", &self.value.path))))
            }
            3 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("sectors", &*self.value.sectors))))
            }
            _ => {
                Ok(None)
            }
        }
    }
}


impl BlockDev {
    pub fn new(path: &Path) -> io::Result<BlockDev> {
        let dev = match Device::from_str(&path.to_string_lossy()) {
            Ok(x) => x,
            Err(_) => return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a block device", path.display())))
        };

        match stat::stat(path) {
            Err(_) => return Err(io::Error::new(
                ErrorKind::NotFound,
                format!("{} not found", path.display()))),
            Ok(x) => x,
        };

        let mut f = match OpenOptions::new().read(true).open(path) {
            Err(_) => {
                return Err(io::Error::new(
                    ErrorKind::PermissionDenied,
                    format!("Could not open {}", path.display())));
            },
            Ok(x) => x,
        };

        let mut buf = [0u8; HEADER_SIZE as usize];
        try!(f.read(&mut buf));

        if &buf[4..20] != FRO_MAGIC {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a Froyo device", path.display())));
        }

        let crc = crc32::checksum_ieee(&buf[4..HEADER_SIZE as usize]);
        if crc != LittleEndian::read_u32(&mut buf[..4]) {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} Froyo header CRC failed", path.display())));
        }

        let sectors = Sectors(try!(blkdev_size(&f)) / SECTOR_SIZE);

        let id = from_utf8(&buf[32..64]).unwrap();

        Ok(BlockDev {
            id: id.to_owned(),
            dev: dev,
            path: path.to_owned(),
            sectors: sectors,
            linear_devs: Vec::new(), // Not initialized until metadata is read
        })
    }

    fn initialize(path: &Path, force: bool) -> io::Result<BlockDev> {
        let pstat = match stat::stat(path) {
            Err(_) => return Err(io::Error::new(
                ErrorKind::NotFound,
                format!("{} not found", path.display()))),
            Ok(x) => x,
        };

        if pstat.st_mode & 0x6000 != 0x6000 {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a block device", path.display())));
        }

        let mut f = match OpenOptions::new().read(true).write(true).open(path) {
            Err(_) => {
                return Err(io::Error::new(
                    ErrorKind::PermissionDenied,
                    format!("Could not open {}", path.display())));
            },
            Ok(x) => x,
        };

        if !force {
            let mut buf = [0u8; 4096];
            try!(f.read(&mut buf));

            if buf.iter().any(|x| *x != 0) {
                return Err(io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("First 4K of {} is not zeroed, need to use --force",
                            path.display())));
            }
        }

        let dev_size = try!(blkdev_size(&f));
        if dev_size < MIN_DEV_SIZE {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} too small, 1G minimum", path.display())));
        }

        let mut buf = [0u8; MDA_ZONE_SIZE as usize];

        copy_memory(FRO_MAGIC, &mut buf[4..20]);
        LittleEndian::write_u64(&mut buf[20..28], dev_size / SECTOR_SIZE);
        // no flags
        let id = Uuid::new_v4().to_simple_string();
        copy_memory(id.as_bytes(), &mut buf[32..64]);
        // no MDAs in use yet

        // All done, calc CRC and write
        let crc = crc32::checksum_ieee(&buf[4..HEADER_SIZE as usize]);
        LittleEndian::write_u32(&mut buf[..4], crc);

        try!(f.seek(SeekFrom::Start(0)));
        try!(f.write_all(&buf));
        try!(f.seek(SeekFrom::End(-(MDA_ZONE_SIZE as i64))));
        try!(f.write_all(&buf));

        try!(f.flush());

        let dev = match Device::from_str(&path.to_string_lossy()) {
            Ok(x) => x,
            Err(_) => return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a block device", path.display())))
        };

        Ok(BlockDev {
            id: id,
            dev: dev,
            path: path.to_owned(),
            sectors: Sectors(dev_size / SECTOR_SIZE),
            linear_devs: Vec::new(),
        })
    }

    fn used_areas(&self) -> Vec<(SectorOffset, Sectors)> {
        let mut used = Vec::new();

        // Flag start and end mda zones as used
        used.push((SectorOffset(0), MDA_ZONE_SECTORS));
        used.push((SectorOffset(*self.sectors - *MDA_ZONE_SECTORS), MDA_ZONE_SECTORS));

        for dev in &self.linear_devs {
            let dev = dev.borrow();
            used.push((dev.start, dev.length))
        }
        used.sort();

        used
    }

    fn free_areas(&self) -> Vec<(SectorOffset, Sectors)> {
        let mut free = Vec::new();

        // Insert an entry to mark the end so the fold works correctly
        let mut used = self.used_areas();
        used.push((SectorOffset(*self.sectors), Sectors(0)));

        used.into_iter()
            .fold(SectorOffset(0), |prev_end, (start, len)| {
                if prev_end < start {
                    free.push((prev_end, Sectors(*start - *prev_end)))
                }
                SectorOffset(*start + *len)
            });

        free
    }

    fn largest_free_area(&self) -> Option<(SectorOffset, Sectors)> {
        self.free_areas().into_iter()
            .max_by(|&(_, len)| len)
    }
}

#[derive(Debug, Clone)]
pub struct LinearDev {
    id: String,
    dev: Device,
    start: SectorOffset,
    length: Sectors,
    parent: Rc<RefCell<BlockDev>>,
}

impl serde::Serialize for LinearDev {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::Serializer
    {
        serializer.visit_struct("LinearDev", LinearDevVisitor {
            value: self,
            state: 0,
        })
    }
}

struct LinearDevVisitor<'a> {
    value: &'a LinearDev,
    state: u8,
}

impl<'a> serde::ser::MapVisitor for LinearDevVisitor<'a> {
    fn visit<S>(&mut self, serializer: &mut S) -> Result<Option<()>, S::Error>
        where S: serde::Serializer
    {
        match self.state {
            0 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("id", &self.value.id))))
            }
            1 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("dev", &self.value.dev))))
            }
            2 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("start", &self.value.start))))
            }
            3 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("length", &self.value.length))))
            }
            4 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("parent", &self.value.parent.borrow().id))))
            }
            _ => {
                Ok(None)
            }
        }
    }
}

impl LinearDev {
    fn create(dm: &DM, name: &str, blockdev: &Rc<RefCell<BlockDev>>, start: SectorOffset, len: Sectors)
              -> io::Result<LinearDev> {

        let dev = blockdev.borrow().dev;
        let params = format!("{}:{} {}",
                             dev.major, dev.minor, *start);
        let table = (0u64, *len, "linear", params.as_ref());

        let dm_name = format!("froyo-linear-{}", name);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let di = try!(dm.table_load(&dm_name, &vec![table][..]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        dbgp!("Created {}", dm_name);

        Ok(LinearDev{
            id: Uuid::new_v4().to_simple_string(),
            dev: di.device(),
            start: start,
            length: len,
            parent: blockdev.clone(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RaidDev {
    dev: Device,
    stripe_sectors: Sectors,
    region_sectors: Sectors,
    length: Sectors,
    members: Vec<(Rc<RefCell<LinearDev>>, Rc<RefCell<LinearDev>>)>,
}

impl serde::Serialize for RaidDev {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::Serializer
    {
        serializer.visit_struct("RaidDev", RaidDevVisitor {
            value: self,
            state: 0,
        })
    }
}

struct RaidDevVisitor<'a> {
    value: &'a RaidDev,
    state: u8,
}

impl<'a> serde::ser::MapVisitor for RaidDevVisitor<'a> {
    fn visit<S>(&mut self, serializer: &mut S) -> Result<Option<()>, S::Error>
        where S: serde::Serializer
    {
        match self.state {
            0 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("dev", &self.value.dev))))
            }
            1 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("stripe_sectors", &self.value.stripe_sectors))))
            }
            2 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("region_sectors", &self.value.region_sectors))))
            }
            3 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("length", &self.value.length))))
            }
            4 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_seq(SeqIteratorVisitor::new(
                    self.value.members.iter()
                        .map(|&(ref x, ref y)| (x.borrow().id.clone(), y.borrow().id.clone())),
                    Some(self.value.members.len()))))))
            }
            _ => {
                Ok(None)
            }
        }
    }
}

impl RaidDev {
    fn create(dm: &DM, name: &str, devs: &[(Rc<RefCell<LinearDev>>, Rc<RefCell<LinearDev>>)], stripe: Sectors, region: Sectors)
              -> io::Result<RaidDev> {

        let raid_texts: Vec<_> = devs.iter()
            .map(|&(ref meta, ref data)| format!("{}:{} {}:{}",
                                          meta.borrow().dev.major, meta.borrow().dev.minor,
                                          data.borrow().dev.major, data.borrow().dev.minor))
            .collect();

        // skip 1 dev to account for parity
        let target_length = devs.iter().skip(1)
            .map(|&(_, ref data)| data.borrow().length)
            .sum::<Sectors>();

        let params = format!("raid5_ls 3 {} region_size {} {} {}",
                             *stripe,
                             *region,
                             raid_texts.len(),
                             raid_texts.join(" "));
        let raid_table = (0u64, *target_length, "raid", &params[..]);

        let dm_name = format!("froyo-raid5-{}", name);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let raid_di = try!(dm.table_load(&dm_name, &vec![raid_table]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        dbgp!("Created {}", dm_name);

        Ok(RaidDev {
            dev: raid_di.device(),
            stripe_sectors: stripe,
            region_sectors: region,
            length: target_length,
            members: devs.to_vec(),
        })
    }
}

#[derive(Debug)]
struct ThinPoolDev {
    dm_name: String,
    dev: Device,
    meta_dev: Device,
    data_dev: Device,
}

impl ThinPoolDev {
    fn create(dm: &DM, name: &str, devs: &[Rc<RefCell<RaidDev>>])
              -> io::Result<ThinPoolDev> {
        // FIXME: Create metadata on 0th raid, data on 1st raid

        let raiddev_0 = devs[0].borrow_mut();
        let raiddev_1 = devs[1].borrow_mut();
        let start = 0;


        let params = format!("{}:{} {}",
                             raiddev_0.dev.major, raiddev_0.dev.minor, start);
        let table = (0u64, 8192, "linear", params.as_ref());

        let dm_name = format!("froyo-linear-thin-meta-{}", name);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let meta_di = try!(dm.table_load(&dm_name, &vec![table]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        dbgp!("Created {}", dm_name);

        let data_sectors = 1024 * 1024;
        let params = format!("{}:{} {}",
                             raiddev_1.dev.major, raiddev_1.dev.minor, start);
        let table = (0u64, data_sectors, "linear", params.as_ref());

        let dm_name = format!("froyo-linear-thin-data-{}", name);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let data_di = try!(dm.table_load(&dm_name, &vec![table]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        dbgp!("Created {}", dm_name);

        let data_block_sectors = 2048; // 1MiB
        let low_water_sectors = 2048 * 512; // 512MiB
        let params = format!("{}:{} {}:{} {} {}",
                             meta_di.device().major, meta_di.device().minor,
                             data_di.device().major, data_di.device().minor,
                             data_block_sectors, low_water_sectors);
        let table = (0u64, data_sectors, "thin-pool", params.as_ref());

        let dm_name = format!("froyo-thin-pool-{}", name);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let pool_di = try!(dm.table_load(&dm_name, &vec![table]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        dbgp!("Created {}", dm_name);

        Ok(ThinPoolDev {
            dm_name: dm_name,
            dev: pool_di.device(),
            meta_dev: meta_di.device(),
            data_dev: data_di.device(),
        })
    }
}

#[derive(Debug, Serialize)]
struct ThinDev {
    dev: Device,
    thin_number: u32,
}

impl ThinDev {
    fn create(dm: &DM, name: &str, pool_dev: &ThinPoolDev) -> io::Result<ThinDev> {
        let thin_number = 669;
        let thin_vol_sectors = 1024 * 1024 * 1024 * 1024 / SECTOR_SIZE;

        let (di, _) = try!(dm.target_msg(&pool_dev.dm_name, 0,
                                         &format!("create_thin {}", thin_number)));

        let params = format!("{}:{} {}", pool_dev.dev.major, pool_dev.dev.minor, thin_number);
        let table = (0u64, thin_vol_sectors, "thin", params.as_ref());

        let dm_name = format!("froyo-thin-{}-{}", name, thin_number);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let thin_di = try!(dm.table_load(&dm_name, &vec![table]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        dbgp!("Created {}", dm_name);

        Ok(ThinDev {
            dev: thin_di.device(),
            thin_number: thin_number,
        })
    }
}

#[derive(Debug)]
pub struct Froyo {
    name: String,
    block_devs: Vec<Rc<RefCell<BlockDev>>>,
    linear_devs: Vec<Rc<RefCell<LinearDev>>>,
    raid_devs: Vec<Rc<RefCell<RaidDev>>>,
    thin_pool_dev: Option<ThinPoolDev>,
    thin_dev: Option<ThinDev>,
}

impl serde::Serialize for Froyo {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::Serializer
    {
        serializer.visit_struct("Froyo", FroyoVisitor {
            value: self,
            state: 0,
        })
    }
}

struct FroyoVisitor<'a> {
    value: &'a Froyo,
    state: u8,
}

impl<'a> serde::ser::MapVisitor for FroyoVisitor<'a> {
    fn visit<S>(&mut self, serializer: &mut S) -> Result<Option<()>, S::Error>
        where S: serde::Serializer
    {
        match self.state {
            0 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("name", &self.value.name))))
            }
            1 => {
                self.state += 1;
                Ok(Some(try!(serializer.visit_struct_elt("thindev", &self.value.thin_dev))))
            }
            _ => {
                Ok(None)
            }
        }
    }
}

impl Froyo {

    fn new(name: &str) -> Froyo {
        Froyo {
            name: name.to_owned(),
            block_devs: Vec::new(),
            linear_devs: Vec::new(),
            raid_devs: Vec::new(),
            thin_pool_dev: None,
            thin_dev: None,
        }
    }

    fn find_all() -> io::Result<Vec<Froyo>> {
        let froyo_bdevs = try!(read_dir("/dev"))
            .into_iter()
            .filter_map(|dir_e| if dir_e.is_ok()
                        { Some(dir_e.unwrap().path()) } else {None} )
            .filter(|path| {
                (stat::stat(path).unwrap().st_mode & 0x6000) == 0x6000 }) // S_IFBLK
            .filter_map(|path| { BlockDev::new(&path).ok() })
            .collect::<Vec<_>>();

        // TODO: build froyodevs out of collected blockdevs.

        Ok(vec![Froyo::new("ss")])
    }


    // Try to make an as-large-as-possible redundant device from the
    // given block devices.
    fn create_redundant_zone(&mut self) -> Result<Option<RaidDev>, FroyoError> {
        let dm = try!(DM::new());

        // TODO: Make sure name has only chars we can use in a DM name

        // get common data area size, allowing for Froyo data at start and end
        let mut bd_areas: Vec<_> = self.block_devs.iter_mut()
            .filter_map(|bd| {
                match bd.borrow().largest_free_area() {
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

        let common_free_sectors = bd_areas.iter()
            .map(|&(_, (_, len))| len)
            .min()
            .unwrap();

        let (region_count, region_sectors) = {
            let mut region_sectors = DEFAULT_REGION_SECTORS;
            while *common_free_sectors / *region_sectors > MAX_REGIONS {
                region_sectors = Sectors(*region_sectors * 2);
            }

            let partial_region = match common_free_sectors % region_sectors == Sectors(0) {
                true => Sectors(0),
                false => Sectors(1),
            };

            (common_free_sectors / region_sectors + partial_region, region_sectors)
        };

        // each region needs 1 bit in the write intent bitmap
        let mdata_sectors = Sectors(align_to(8192 + (*region_count / 8) , SECTOR_SIZE)
                                    .next_power_of_two()
                                    / SECTOR_SIZE);
        // data size must be multiple of stripe size
        let data_sectors = (common_free_sectors - mdata_sectors) & Sectors(!(*STRIPE_SECTORS-1));

        let raid_num = self.raid_devs.len();

        let mut linear_dev_pairs = Vec::new();
        for (num, &mut(ref mut bd, (sector_start, _))) in bd_areas.iter_mut().enumerate() {
            let mdata_sector_start = sector_start;
            let data_sector_start = SectorOffset(*mdata_sector_start + *mdata_sectors);

            let meta = Rc::new(RefCell::new(try!(LinearDev::create(
                &dm,
                &format!("{}-meta-{}-{}", self.name, raid_num, num),
                bd,
                mdata_sector_start,
                mdata_sectors))));
            bd.borrow_mut().linear_devs.push(meta.clone());

            let data = Rc::new(RefCell::new(try!(LinearDev::create(
                &dm,
                &format!("{}-data-{}-{}", self.name, raid_num, num),
                bd,
                data_sector_start,
                data_sectors))));
            bd.borrow_mut().linear_devs.push(data.clone());

            linear_dev_pairs.push((meta, data));
        }

        let raid = try!(RaidDev::create(
            &dm,
            &format!("{}-{}", self.name, raid_num),
            &linear_dev_pairs[..],
            STRIPE_SECTORS,
            region_sectors));

        Ok(Some(raid))
    }

    pub fn create_redundant_zones(&mut self) -> Result<(), FroyoError> {
        loop {
            if let Some(rd) = try!(self.create_redundant_zone()) {
                self.raid_devs.push(Rc::new(RefCell::new(rd)));
            } else {
                break
            }
        }

        Ok(())
    }

    fn add_blockdev(&mut self, bdev: BlockDev) -> Result<(), FroyoError> {
        self.block_devs.push(Rc::new(RefCell::new(bdev)));

        Ok(())
    }

    pub fn reshape(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn list(_args: &ArgMatches) -> Result<(), FroyoError> {
    println!("hello from list()");
    Ok(())
}

fn status(_args: &ArgMatches) -> Result<(), FroyoError> {
    println!("hello from status()");
    Ok(())
}

fn add(_args: &ArgMatches) -> Result<(), FroyoError> {
    println!("hello from add()");
    Ok(())
}

fn remove(_args: &ArgMatches) -> Result<(), FroyoError> {
    println!("hello from remove()");
    Ok(())
}

fn create(args: &ArgMatches) -> Result<(), FroyoError> {
    let name = args.value_of("froyodevname").unwrap();
    let dev_paths: Vec<_> = args.values_of("devices").unwrap().into_iter()
        .map(|dev| {
            if !Path::new(dev).is_absolute() {
                PathBuf::from(format!("/dev/{}", dev))
            } else {
                PathBuf::from(dev)
            }})
        .collect();

    if dev_paths.len() < 2 {
        return Err(FroyoError::Io(io::Error::new(
            ErrorKind::InvalidInput, "At least 2 block devices must be given")))
    }

    if dev_paths.len() > 8 {
        return Err(FroyoError::Io(io::Error::new(
            ErrorKind::InvalidInput,
            format!("Max supported devices is 8, {} given", dev_paths.len()))))
    }

    let mut froyo = Froyo::new(name);

    for pathbuf in dev_paths {
        let bd = try!(BlockDev::initialize(&pathbuf, args.is_present("force")));
        println!("sss {}", try!(serde_json::to_string(&bd)));
        try!(froyo.add_blockdev(bd));
    }

    try!(froyo.create_redundant_zones());

    let dm = try!(DM::new());

    let thin_pool_dev = try!(ThinPoolDev::create(&dm, name, &froyo.raid_devs));
    let thin_dev = try!(ThinDev::create(&dm, name, &thin_pool_dev));

    froyo.thin_pool_dev = Some(thin_pool_dev);
    froyo.thin_dev = Some(thin_dev);

    // TODO: write metadata to all disks
    // println!("sss {}", try!(serde_json::to_string(&froyo)));

    Ok(())
}

fn main() {

    let matches = App::new("froyo")
        .version(&crate_version!())
        .author("Andy Grover <andy@groveronline.com>")
        .about("Drobo + Free + YOLO")
        .arg(Arg::with_name("debug")
             .short("d")
             .long("debug")
             .help("Print additional output for debugging")
             )
        .subcommand(SubCommand::with_name("list")
                    .about("List all froyodevs")
                    .arg(Arg::with_name("long")
                         .short("l")
                         .long("long")
                         .help("Use a long listing format")
                         )
                    )
        .subcommand(SubCommand::with_name("status")
                    .about("Get the status of a single froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .required(true)
                         .help("Froyodev to get info on")
                         .index(1)
                         )
                    )
        .subcommand(SubCommand::with_name("add")
                    .about("Add one or more additional block devices to a froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .help("Froyodev to add the device to")
                         .required(true)
                         .index(1)
                         )
                    .arg(Arg::with_name("devices")
                         .help("device(s) to add")
                         .multiple(true)
                         .required(true)
                         .index(2)
                         )
                    )
        .subcommand(SubCommand::with_name("remove")
                    .about("Remove a block device from a froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .help("Froyodev to remove the device from")
                         .required(true)
                         .index(1)
                         )
                    .arg(Arg::with_name("devices")
                         .help("Block device(s) to remove")
                         .multiple(true)
                         .required(true)
                         .index(2)
                         )
                    )
        .subcommand(SubCommand::with_name("create")
                    .about("Create a new froyodev")
                    .arg(Arg::with_name("force")
                         .short("f")
                         .long("force")
                         .help("Force")
                         )
                    .arg(Arg::with_name("froyodevname")
                         .help("Name of the new froyodev")
                         .required(true)
                         .index(1)
                         )
                    .arg(Arg::with_name("devices")
                         .help("Initial block device(s) to use")
                         .multiple(true)
                         .required(true)
                         .index(2)
                         )
                    )
        .get_matches();

    if matches.is_present("debug") {
        // must use unsafe to change a mut static, sigh
        unsafe { debug = true };
    }

    let r = match matches.subcommand() {
        ("list", Some(matches)) => list(matches),
        ("status", Some(matches)) => status(matches),
        ("add", Some(matches)) => add(matches),
        ("remove", Some(matches)) => remove(matches),
        ("create", Some(matches)) => create(matches),
        ("", None) => {
            println!("No command given, try \"help\"");
            Ok(())
        }
        _ => unreachable!(),
    };

    if let Err(r) = r {
        match writeln!(&mut ::std::io::stderr(), "{}", r.description()) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
        exit(1);
    }
}
