// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(iter_arith, zero_one, custom_derive,
           custom_attribute, plugin, const_fn)]
#![plugin(serde_macros)]
#![plugin(clippy)]

#![allow(match_same_arms)] // we seem to have instances where same arms are good
#![allow(dead_code)] // only temporary, until more stuff is filled in

extern crate devicemapper;
#[macro_use] extern crate clap;
#[macro_use] extern crate nix;
extern crate crc;
extern crate byteorder;
extern crate uuid;
extern crate time;
extern crate serde;
extern crate serde_json;
extern crate bytesize;
extern crate dbus;
extern crate term;

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
mod dbus_api;

use std::io::Write;
use std::error::Error;
use std::process::exit;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::cell::RefCell;

use clap::{App, Arg, SubCommand, ArgMatches};
use bytesize::ByteSize;
use dbus::{Connection, BusType};

use types::FroyoResult;
use consts::SECTOR_SIZE;
use froyo::{Froyo, FroyoStatus};



// We are given BlockDevs to start.
// We allocate LinearDevs from each for the meta and data devices.
// We use all these to make RaidDevs.
// We create two RaidLinearDevs from these for meta and data devices.
// We use these to make a ThinPoolDev.
// From that, we allocate a ThinDev.

fn list(_args: &ArgMatches) -> FroyoResult<()> {
    let froyos = try!(Froyo::find_all());
    for f in &froyos {
        println!("{}", f.name);
    }

    Ok(())
}

fn status(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    match try!(Froyo::find(&name)) {
        Some(f) => {
            let status = match try!(f.status()) {
                FroyoStatus::Good(_) => "good",
                FroyoStatus::RaidFailed => "RAID failed",
                FroyoStatus::ThinPoolFailed => "Thin pool failed",
                FroyoStatus::ThinFailed => "Thin device failed",
            };

            let space = *try!(f.avail_redundant_space()) * SECTOR_SIZE;
            let total = *f.total_redundant_space() * SECTOR_SIZE;

            let percent = ((total-space) * 100) / total;

            println!("Status: {}, {}% used ({} of {} free)",
                     status, percent,
                     ByteSize::b(space as usize).to_string(true),
                     ByteSize::b(total as usize).to_string(true));
        },
        None => println!("Froyodev \"{}\" not found", name),
    }
    Ok(())
}

fn add(_args: &ArgMatches) -> FroyoResult<()> {
    println!("hello from add()");
    Ok(())
}

fn remove(_args: &ArgMatches) -> FroyoResult<()> {
    let mut out = term::stdout().expect("could not get stdout");

    try!(out.fg(term::color::GREEN));
    try!(writeln!(out, "hello color"));
    try!(out.reset());

    Ok(())
}

fn create(args: &ArgMatches) -> FroyoResult<()> {
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

    let froyo = try!(Froyo::new(name, &dev_paths, force));

    try!(froyo.save_state());

    dbgp!("Froyodev {} created", froyo.name);

    Ok(())
}

fn dump_meta(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    match try!(Froyo::find(&name)) {
        Some(f) =>
            println!("{}", try!(f.to_metadata_pretty())),
        None => println!("Froyodev \"{}\" not found", name),
    }

    Ok(())
}

fn dbus_server(_args: &ArgMatches) -> FroyoResult<()> {
    let c = Connection::get_private(BusType::Session).unwrap();
    let froyos = try!(Froyo::find_all());
    let froyos = froyos.into_iter()
        .map(|f| Rc::new(RefCell::new(f)))
        .collect::<Vec<_>>();
    let mut froyos = Rc::new(RefCell::new(froyos));
    let tree = try!(dbus_api::get_tree(&c, &mut froyos));

    // TODO: event loop needs to handle dbus and also dm events (or polling)
    // so we can extend/reshape/delay/whatever in a timely fashion
    for _ in tree.run(&c, c.iter(1000)) {

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
        .subcommand(SubCommand::with_name("dev")
                    .about("Developer/debug commands")
                    .subcommand(SubCommand::with_name("dump_meta")
                                .about("Output the JSON metadata for a froyodev")
                                .arg(Arg::with_name("froyodevname")
                                     .help("Name of the froyodev")
                                     .required(true)
                                     .index(1)
                                     )
                                )
                    .subcommand(SubCommand::with_name("dbus_server")
                                .about("Serve the Froyo DBus API")
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
        ("dev", Some(matches)) => match matches.subcommand() {
            ("dump_meta", Some(matches)) => dump_meta(matches),
            ("dbus_server", Some(matches)) => dbus_server(matches),
            ("", None) => {
                println!("No command given, try \"help\"");
                Ok(())
            }
            _ => unreachable!(),
        },
        ("", None) => {
            println!("No command given, try \"help\"");
            Ok(())
        }
        _ => unreachable!(),
    };

    if let Err(r) = r {
        if let Err(e) = writeln!(&mut ::std::io::stderr(), "{}", r.description()) {
            panic!("Unable to write to stderr: {}", e)
        }
        exit(1);
    }
}
