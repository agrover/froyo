// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(slice_bytes, custom_derive, plugin)]
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

#[allow(unused_imports)]
use std::io;
use std::io::{Read, Write, ErrorKind, Seek, SeekFrom};
use std::error::Error;
use std::process::exit;
use std::fs::{File, OpenOptions};
use std::path::{Path, PathBuf};
use std::str::{FromStr, from_utf8};
use std::slice::bytes::copy_memory;
use std::os::unix::prelude::AsRawFd;
use std::fmt;

use devicemapper::{DM, Device, DmFlags};
use clap::{App, Arg, SubCommand, ArgMatches};
use nix::sys::{stat, ioctl};
use crc::crc32;
use byteorder::{LittleEndian, ByteOrder};
use uuid::Uuid;

const SECTOR_SIZE: usize = 512;
const HEADER_SIZE: usize = 512;
const MDA_ZONE_SIZE: usize = (1024 * 1024);
const MDA_ZONE_SECTORS: usize = (MDA_ZONE_SIZE / SECTOR_SIZE);
const FRO_MAGIC: &'static [u8] = b"!IamFroy0\x86\xffGO\x02^\x41";
const STRIPE_SECTORS: usize = 2048;

// No devs smaller than around a gig
const MIN_DATA_ZONE_SIZE: usize = (1024 * 1024 * 1024);
const MIN_DATA_ZONE_SECTORS: usize = MIN_DATA_ZONE_SIZE / SECTOR_SIZE;
const MIN_DEV_SIZE: usize = MIN_DATA_ZONE_SIZE + (2 * MDA_ZONE_SIZE);
const MIN_DEV_SECTORS: usize = MIN_DEV_SIZE / SECTOR_SIZE;

const MAX_REGIONS: usize = (2 * 1024 * 1024);
const DEFAULT_REGION_SIZE: usize = (4 * 1024 * 1024);
const DEFAULT_REGION_SECTORS: usize = DEFAULT_REGION_SIZE / SECTOR_SIZE;

static mut debug: bool = false;

macro_rules! dbgp {
    ($($arg:tt)*) => (
        unsafe {
            if debug {
                println!($($arg)*)
            }
        })
}

macro_rules! errp {
    ($($arg:tt)*) => (
        match writeln!(&mut ::std::io::stderr(), $($arg)* ) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        })
}

fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}

// Define a common error enum.
// See http://blog.burntsushi.net/rust-error-handling/
#[derive(Debug)]
enum FroyoError {
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

#[derive(Debug, Clone, Serialize)]
pub struct FroyoDev {
    id: String,
    dev: Device,
    path: PathBuf,
    sector_count: usize,
}

impl FroyoDev {
    pub fn new(path: &Path) -> io::Result<FroyoDev> {
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

        let mut buf = [0u8; HEADER_SIZE];
        try!(f.read(&mut buf));

        if &buf[4..20] != FRO_MAGIC {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a Froyo device", path.display())));
        }

        let crc = crc32::checksum_ieee(&buf[4..HEADER_SIZE]);
        if crc != LittleEndian::read_u32(&mut buf[..4]) {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} Froyo header CRC failed", path.display())));
        }

        let sector_count = try!(blkdev_size(&f)) as usize / SECTOR_SIZE;

        let id = from_utf8(&buf[32..64]).unwrap();

        Ok(FroyoDev {id: id.to_owned(), dev: dev, path: path.to_owned(), sector_count: sector_count})
    }

    fn initialize(path: &Path, force: bool) -> io::Result<FroyoDev> {
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
        if dev_size < MIN_DEV_SIZE as u64 {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} too small, 1G minimum", path.display())));
        }

        let mut buf = [0u8; MDA_ZONE_SIZE];

        copy_memory(FRO_MAGIC, &mut buf[4..20]);
        LittleEndian::write_u64(&mut buf[20..28], dev_size / SECTOR_SIZE as u64);
        // no flags
        let id = Uuid::new_v4().to_simple_string();
        copy_memory(id.as_bytes(), &mut buf[32..64]);
        // no MDAs in use yet

        // All done, calc CRC and write
        let crc = crc32::checksum_ieee(&buf[4..HEADER_SIZE]);
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

        Ok(FroyoDev {
            id: id,
            dev: dev,
            path: path.to_owned(),
            sector_count: dev_size as usize / SECTOR_SIZE})
    }
}

#[derive(Debug, Serialize)]
struct Froyo {
    name: String,
    devs: Vec<FroyoDev>,
}

impl Froyo {
    fn create(name: &str, fds: Vec<FroyoDev>) -> Result<Froyo, FroyoError> {

        let dm = try!(DM::new());

        // TODO: Make sure name has only chars we can use in a DM name
        // TODO: filter devs based on minimum free space

        // get common data area size, allowing for Froyo data at start and end
        let common_free_sectors = fds.iter()
            .map(|fd| fd.sector_count - (2 * MDA_ZONE_SECTORS))
            .min()
            .expect("should never happen");

        let (region_count, region_sectors) = {
            let mut region_sectors = DEFAULT_REGION_SECTORS;
            while common_free_sectors / region_sectors > MAX_REGIONS {
                region_sectors *= 2;
            }

            let partial_region = match common_free_sectors % region_sectors == 0 {
                true => 0,
                false => 1,
            };

            (common_free_sectors / region_sectors + partial_region, region_sectors)
        };

        let mdata_sector_start = MDA_ZONE_SECTORS;
        // each region needs 1 bit in the write intent bitmap
        let mdata_sectors = align_to(8192 + (region_count / 8), SECTOR_SIZE)
            .next_power_of_two()
            / SECTOR_SIZE;
        let data_sector_start = mdata_sector_start + mdata_sectors;
        // data size must be multiple of stripe size
        let data_sectors = (common_free_sectors - mdata_sectors) & !(STRIPE_SECTORS-1);

        println!("STUFF {} {} {} {} {}",
                 common_free_sectors,
                 mdata_sector_start,
                 mdata_sectors,
                 data_sector_start,
                 data_sectors);

        // Create metadata and data devices for raid from each dev
        let mut layer1_devices = Vec::new();
        for (num, fd) in fds.iter().enumerate() {
            let params = format!("{}:{} {}",
                                 fd.dev.major, fd.dev.minor, mdata_sector_start);
            let mdata_table = (0u64, mdata_sectors as u64, "linear", params.as_ref());

            let dm_name = format!("froyo-base-mdata-{}-{}", name, num);

            try!(dm.device_create(&dm_name, None, DmFlags::empty()));
            let mdata_di = try!(dm.table_load(&dm_name, &vec![mdata_table]));
            try!(dm.device_suspend(&dm_name, DmFlags::empty()));

            let params = format!("{}:{} {}",
                                 fd.dev.major, fd.dev.minor, data_sector_start);
            let data_table = (0u64, data_sectors as u64, "linear", &params[..]);

            let dm_name = format!("froyo-base-data-{}-{}", name, num);

            try!(dm.device_create(&dm_name, None, DmFlags::empty()));
            let data_di = try!(dm.table_load(&dm_name, &vec![data_table]));
            try!(dm.device_suspend(&dm_name, DmFlags::empty()));

            layer1_devices.push((mdata_di.device(), data_di.device()));
        }

        // TODO create raid based on minimum size of all constituent devs

        let raid_devs: Vec<_> = layer1_devices.iter()
            .map(|&(mdata, data)| format!("{}:{} {}:{}",
                                         mdata.major, mdata.minor,
                                         data.major, data.minor))
            .collect();

        let target_length: u64 = data_sectors as u64 * (raid_devs.len() as u64 - 1);
        let params = format!("raid5_ls 3 {} region_size {} {} {}",
                             STRIPE_SECTORS,
                             region_sectors,
                             raid_devs.len(),
                             raid_devs.join(" "));
        let raid_table = (0u64, target_length, "raid", &params[..]);
        println!("TABLE: {:?}", raid_table);

        let dm_name = format!("froyo-raid5-{}-{}", name, 0);

        try!(dm.device_create(&dm_name, None, DmFlags::empty()));
        let raid_di = try!(dm.table_load(&dm_name, &vec![raid_table]));
        try!(dm.device_suspend(&dm_name, DmFlags::empty()));

        Ok(Froyo {
            name: name.to_string(),
            devs: fds,
        })
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

    let mut fds = Vec::new();
    for pathbuf in dev_paths {
        dbgp!("Initializing {}", &pathbuf.display());
        let fd = try!(FroyoDev::initialize(&pathbuf, args.is_present("force")));
        fds.push(fd);
    }

    // TODO: Build froyodev on top of our newly created blockdevs
    let froyo = try!(Froyo::create(name, fds));

    dbgp!("Created {}", name);
    println!("sss {}", try!(serde_json::to_string(&froyo)));

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
        errp!("{}", r.description());
        exit(1);
    };
}
