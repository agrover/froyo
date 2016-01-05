// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(iter_arith, zero_one, custom_derive,
           custom_attribute, plugin, clone_from_slice, const_fn)]
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

pub static mut debug: bool = false;

macro_rules! dbgp {
    ($($arg:tt)*) => (
        unsafe {
            if ::debug {
                println!($($arg)*)
            }
        })
}

mod types;
mod consts;
mod froyo;
mod blockdev;
mod raid;
mod thin;
mod util;

use std::io::Write;
use std::error::Error;
use std::process::exit;
use std::path::{Path, PathBuf};

use clap::{App, Arg, SubCommand, ArgMatches};
use uuid::Uuid;

use types::FroyoError;
use froyo::{Froyo, FroyoStatus, FroyoPerfStatus};



// We are given BlockDevs to start.
// We allocate LinearDevs from each for the meta and data devices.
// We use all these to make RaidDevs.
// We create two RaidLinearDevs from these for meta and data devices.
// We use these to make a ThinPoolDev.
// From that, we allocate a ThinDev.

fn list(_args: &ArgMatches) -> Result<(), FroyoError> {
    let froyos = try!(Froyo::find_all());
    for f in &froyos {
        println!("{}", f.name);
    }

    Ok(())
}

fn status(args: &ArgMatches) -> Result<(), FroyoError> {
    let name = args.value_of("froyodevname").unwrap();
    match try!(Froyo::find(&name)) {
        Some(f) => {
            let (status, perf_status, (a, b)) = try!(f.status());

            let status = match status {
                FroyoStatus::Good => "good",
                FroyoStatus::Degraded(_) => "degraded",
                FroyoStatus::Failed => "failed",
            };

            let perf_status = match perf_status {
                FroyoPerfStatus::Good => "not throttled",
                FroyoPerfStatus::Throttled => "throttled",
            };

            println!("{}: {}, {} {}/{}", f.name, status, perf_status, *a, *b);
        },
        None => println!("Froyodev \"{}\" not found", name),
    }
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
    let force = args.is_present("force");

    let froyo = try!(Froyo::create(name, &Uuid::new_v4().to_simple_string(), &dev_paths, force));

    try!(froyo.save_state());

    dbgp!("Froyodev {} created", froyo.name);

    Ok(())
}

fn dump_meta(args: &ArgMatches) -> Result<(), FroyoError> {
    let name = args.value_of("froyodevname").unwrap();
    match try!(Froyo::find(&name)) {
        Some(f) =>
            println!("{}", try!(f.to_metadata_pretty())),
        None => println!("Froyodev \"{}\" not found", name),
    }

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
        .subcommand(SubCommand::with_name("dump_meta")
                    .about("Output the JSON metadata for a froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .help("Name of the froyodev")
                         .required(true)
                         .index(1)
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
        ("dump_meta", Some(matches)) => dump_meta(matches),
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
