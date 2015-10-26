// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate devicemapper;
#[macro_use]
extern crate clap;
extern crate nix;

#[allow(unused_imports)]
use std::io;
use std::io::{Read, Write, ErrorKind};
use std::error::Error;
use std::process::exit;
use std::fs::OpenOptions;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use devicemapper as devm;
use devicemapper::Device;
use clap::{App, Arg, SubCommand, ArgMatches};
use nix::sys::stat;

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

struct FroyoDev {
    dev: Device,
}

impl FroyoDev {
    fn new(dev: Device) -> io::Result<FroyoDev> {
        unimplemented!()
    }

    fn initialize(f: &mut Froyo, dev: Device, force: bool) -> io::Result<FroyoDev> {
        let pathbuf = try!(dev.path().ok_or(io::Error::new(
            ErrorKind::NotFound,
            format!("Could not get path for {}:{}", dev.major, dev.minor))));


        let pstat = match stat::stat(&pathbuf) {
            Err(_) => return Err(io::Error::new(
                ErrorKind::NotFound,
                format!("{} not found", &pathbuf.display()))),
            Ok(x) => x,
        };

        if pstat.st_mode & 0x6000 != 0x6000 {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a block device", &pathbuf.display())));
        }

        let mut f = match OpenOptions::new().read(true).write(true).open(&pathbuf) {
            Err(_) => {
                return Err(io::Error::new(
                    ErrorKind::PermissionDenied,
                    format!("Could not open {}", &pathbuf.display())));
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
                            &pathbuf.display())));
            }
        }

        println!("initialize the blockdev!");

        Ok(FroyoDev {dev: dev})
    }
}

struct Froyo {
    name: String,
    devs: Vec<FroyoDev>,
}

impl Froyo {
    fn new(name: &str) -> Froyo {
        Froyo {
            name: name.to_string(),
            devs: Vec::new(),
        }
    }
}

fn list(args: &ArgMatches) -> io::Result<()> {
    println!("hello from list()");
    Ok(())
}

fn status(args: &ArgMatches) -> io::Result<()> {
    println!("hello from status()");
    Ok(())
}

fn add(args: &ArgMatches) -> io::Result<()> {
    println!("hello from add()");
    Ok(())
}

fn remove(args: &ArgMatches) -> io::Result<()> {
    println!("hello from remove()");
    Ok(())
}

fn create(args: &ArgMatches) -> io::Result<()> {
    let name = args.value_of("froyodevname").unwrap();
    let dev_paths: Vec<_> = args.values_of("devices").unwrap().into_iter()
        .map(|dev| {
            if !Path::new(dev).is_absolute() {
                PathBuf::from(format!("/dev/{}", dev))
            } else {
                PathBuf::from(dev)
            }})
        .collect();

    let mut froyo = Froyo::new(name);

    for path in dev_paths {
        let dev = match Device::from_str(&path.to_string_lossy()) {
            Ok(x) => x,
            Err(_) => return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a block device", path.display())))
        };

        dbgp!("Initializing {}", &path.display());
        let fd = try!(FroyoDev::initialize(&mut froyo, dev, args.is_present("force")));
    }

    // TODO: Build froyodev on top of our newly created blockdevs

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
