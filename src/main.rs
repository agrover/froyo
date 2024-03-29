// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![allow(dead_code)] // only temporary, until more stuff is filled in

// #[macro_use]
// extern crate custom_derive;
// #[macro_use]
// extern crate newtype_derive;

pub static mut DEBUG: bool = false;

macro_rules! dbgp {
    ($($arg:tt)*) => (
        unsafe {
            if crate::DEBUG {
                println!($($arg)*)
            }
        })
}

mod blockdev;
mod consts;
mod dbus_api;
mod dmdevice;
mod froyo;
mod mirror;
mod raid;
mod serialize;
mod thin;
mod types;
mod util;

use std::borrow::Cow;
use std::cell::RefCell;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::rc::Rc;

use bytesize::ByteSize;
use chrono::{Duration, Utc, MIN_DATETIME};
use clap::{crate_version, App, Arg, ArgMatches, SubCommand};
use dbus::{BusType, Connection, FromMessageItem, Message, MessageItem, Props};
use dbus::{ConnectionItem, MessageType};

use consts::{DBUS_TIMEOUT, SECTOR_SIZE};
use froyo::Froyo;
use types::{FroyoError, FroyoResult, InternalError};

// We are given BlockDevs to start.
// We allocate LinearDevs from each for the meta and data devices.
// We use all these to make RaidDevs.
// We create two RaidLinearDevs from these for meta and data devices.
// We use these to make a ThinPoolDev.
// From that, we allocate a ThinDev.

trait FroyoDbusConnection {
    fn froyo_connect() -> FroyoResult<Connection>;
    fn froyo_paths(&self) -> FroyoResult<Vec<String>>;
    fn froyo_path(&self, name: &str) -> FroyoResult<String>;
}

impl FroyoDbusConnection for Connection {
    fn froyo_connect() -> FroyoResult<Connection> {
        let c = Connection::get_private(BusType::Session)?;
        Ok(c)
    }
    fn froyo_paths(&self) -> FroyoResult<Vec<String>> {
        let m = Message::new_method_call(
            "org.freedesktop.Froyo1",
            "/org/freedesktop/froyodevs",
            "org.freedesktop.DBus.ObjectManager",
            "GetManagedObjects",
        )
        .unwrap();
        let r = self.send_with_reply_and_block(m, DBUS_TIMEOUT)?;
        let reply = r.get_items();

        let mut froyos = Vec::new();
        let array: &Vec<MessageItem> = FromMessageItem::from(&reply[0]).unwrap();
        for item in array {
            let (k, _) = FromMessageItem::from(item).unwrap();
            let kstr: &str = FromMessageItem::from(k).unwrap();
            if kstr != "/org/freedesktop/froyodevs" {
                froyos.push(kstr.to_owned());
            }
        }
        Ok(froyos)
    }

    fn froyo_path(&self, name: &str) -> FroyoResult<String> {
        let froyos = self.froyo_paths()?;

        for fpath in &froyos {
            let p = Props::new(
                self,
                "org.freedesktop.Froyo1",
                fpath,
                "org.freedesktop.FroyoDevice1",
                DBUS_TIMEOUT,
            );
            let item = p.get("Name").unwrap();
            let froyo_name: &str = FromMessageItem::from(&item).unwrap();
            if name == froyo_name {
                return Ok(fpath.to_owned());
            }
        }

        Err(FroyoError::Froyo(InternalError(
            format!("Froyodev \"{}\" not found", name).into(),
        )))
    }
}

fn list(_args: &ArgMatches) -> FroyoResult<()> {
    let c = Connection::froyo_connect()?;
    let froyos = c.froyo_paths()?;

    for fpath in &froyos {
        let p = Props::new(
            &c,
            "org.freedesktop.Froyo1",
            fpath,
            "org.freedesktop.FroyoDevice1",
            DBUS_TIMEOUT,
        );
        let item = p.get("Name")?;
        let name: &str = FromMessageItem::from(&item).unwrap();
        println!("{}", name);
    }

    Ok(())
}

fn status(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    let c = Connection::froyo_connect()?;
    let fpath = c.froyo_path(name)?;
    let p = Props::new(
        &c,
        "org.freedesktop.Froyo1",
        fpath,
        "org.freedesktop.FroyoDevice1",
        DBUS_TIMEOUT,
    );
    let status_msg = p.get("Status")?;
    let status: u32 = FromMessageItem::from(&status_msg).unwrap();
    let r_status_msg = p.get("RunningStatus")?;
    let r_status: u32 = FromMessageItem::from(&r_status_msg).unwrap();

    let stat_str: Cow<str> = {
        if status != 0 {
            let mut stats: Vec<Cow<_>> = Vec::new();
            if 0xff & status != 0 {
                stats.push(format!("stopped, need {} blockdevs", status & 0xff).into())
            }
            if 0x100 & status != 0 {
                stats.push("RAID failure".into())
            }
            if 0x200 & status != 0 {
                stats.push("Thin pool failure: metadata".into())
            }
            if 0x400 & status != 0 {
                stats.push("Thin pool failure: data".into())
            }
            if 0x800 & status != 0 {
                stats.push("Thin device failure".into())
            }
            if 0x1000 & status != 0 {
                stats.push("Filesystem failure".into())
            }
            if 0x2000 & status != 0 {
                stats.push("Initializing".into())
            }
            if 0xffffc000 & status != 0 {
                stats.push(format!("Unenumerated failure: {:x}", status).into())
            }
            stats.join(", ").into()
        } else if r_status != 0 {
            let mut stats: Vec<Cow<_>> = Vec::new();
            if 0xff & r_status != 0 {
                stats.push(format!("missing {} blockdevs", r_status & 0xff).into())
            }
            if 0x100 & r_status != 0 {
                stats.push("Non-redundant".into())
            }
            if 0x200 & r_status != 0 {
                stats.push("Cannot reshape".into())
            } else {
                stats.push("Can reshape".into())
            }
            if 0x400 & r_status != 0 {
                stats.push("Reshaping".into())
            }
            if 0x800 & r_status != 0 {
                stats.push("Throttled".into())
            }
            if 0xfffff000 & r_status != 0 {
                stats.push(format!("Unenumerated issue: {:x}", r_status).into())
            }
            stats.join(", ").into()
        } else {
            "Running".into()
        }
    };

    let space_msg = p.get("RemainingSectors")?;
    let space: u64 = FromMessageItem::from(&space_msg).unwrap();
    let space = space * SECTOR_SIZE;

    let total_msg = p.get("TotalSectors")?;
    let total: u64 = FromMessageItem::from(&total_msg).unwrap();
    let total = total * SECTOR_SIZE;

    let percent = ((total - space) * 100) / total;

    println!(
        "Status: {}, {}% used ({} of {} free)",
        stat_str,
        percent,
        ByteSize::b(space as usize).to_string(true),
        ByteSize::b(total as usize).to_string(true)
    );

    let err_msg = "Unexpected format of BlockDevices property";
    let bdevs = p.get("BlockDevices")?;
    let bdev_vec: &Vec<_> = bdevs
        .inner()
        .map_err(|_| FroyoError::Froyo(InternalError(err_msg.into())))?;
    println!("Member devices:");
    for bdev in bdev_vec {
        let inner_vals: &Vec<_> = bdev
            .inner()
            .map_err(|_| FroyoError::Froyo(InternalError(err_msg.into())))?;
        let name: &str = inner_vals[0]
            .inner()
            .map_err(|_| FroyoError::Froyo(InternalError(err_msg.into())))?;
        let status: u32 = inner_vals[1]
            .inner()
            .map_err(|_| FroyoError::Froyo(InternalError(err_msg.into())))?;
        let status_str = match status {
            0 => "In use",
            1 => "Not in use",
            2 => "Bad",
            3 => "Not present",
            _ => "Unknown",
        };
        println!("{} {}", name, status_str);
    }

    Ok(())
}

fn add(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    let dev_paths: Vec<_> = args
        .values_of("devices")
        .unwrap()
        .into_iter()
        .map(|dev| {
            if Path::new(dev).is_absolute() {
                PathBuf::from(dev)
            } else {
                PathBuf::from(format!("/dev/{}", dev))
            }
        })
        .collect();
    let force = args.is_present("force");
    let c = Connection::froyo_connect()?;
    let fpath = c.froyo_path(name)?;

    for path in dev_paths {
        let mut m = Message::new_method_call(
            "org.freedesktop.Froyo1",
            &fpath,
            "org.freedesktop.FroyoDevice1",
            "AddBlockDevice",
        )
        .unwrap();
        m.append_items(&[path.to_string_lossy().into_owned().into(), force.into()]);
        c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;
    }

    Ok(())
}

fn remove(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    let bd_path = {
        let dev = args.value_of("blockdev").unwrap();
        if Path::new(dev).is_absolute() {
            PathBuf::from(dev)
        } else {
            PathBuf::from(format!("/dev/{}", dev))
        }
    };
    let wipe = args.is_present("wipe");
    let c = Connection::froyo_connect()?;
    let fpath = c.froyo_path(name)?;

    let mut m = Message::new_method_call(
        "org.freedesktop.Froyo1",
        &fpath,
        "org.freedesktop.FroyoDevice1",
        "RemoveBlockDevice",
    )
    .unwrap();
    m.append_items(&[bd_path.to_string_lossy().into_owned().into(), wipe.into()]);
    c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;

    Ok(())
}

fn create(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    let dev_paths: Vec<_> = args
        .values_of("devices")
        .unwrap()
        .into_iter()
        .map(|dev| {
            if Path::new(dev).is_absolute() {
                PathBuf::from(dev)
            } else {
                PathBuf::from(format!("/dev/{}", dev))
            }
        })
        .map(|pb| pb.to_string_lossy().into_owned().into())
        .collect();
    let force = args.is_present("force");

    let c = Connection::froyo_connect()?;

    let mut m = Message::new_method_call(
        "org.freedesktop.Froyo1",
        "/org/freedesktop/froyo",
        "org.freedesktop.FroyoService1",
        "Create",
    )
    .unwrap();
    m.append_items(&[
        name.into(),
        MessageItem::new_array(dev_paths).unwrap(),
        force.into(),
    ]);
    c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;

    dbgp!("Froyodev {} created", name);

    Ok(())
}

fn rename(args: &ArgMatches) -> FroyoResult<()> {
    let old_name = args.value_of("froyodev_old_name").unwrap();
    let new_name = args.value_of("froyodev_new_name").unwrap();

    let c = Connection::froyo_connect()?;
    let fpath = c.froyo_path(old_name)?;

    let mut m = Message::new_method_call(
        "org.freedesktop.Froyo1",
        &fpath,
        "org.freedesktop.FroyoDevice1",
        "SetName",
    )
    .unwrap();
    m.append_items(&[new_name.into()]);
    c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;

    dbgp!("Froyodev name {} changed to {}", old_name, new_name);

    Ok(())
}

fn reshape(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodev").unwrap();

    let c = Connection::froyo_connect()?;
    let fpath = c.froyo_path(name)?;

    let m = Message::new_method_call(
        "org.freedesktop.Froyo1",
        &fpath,
        "org.freedesktop.FroyoDevice1",
        "Reshape",
    )
    .unwrap();
    c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;

    dbgp!("Froyodev {} starting reshape", name);

    Ok(())
}

fn destroy(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodev").unwrap();

    let c = Connection::froyo_connect()?;

    let mut m = Message::new_method_call(
        "org.freedesktop.Froyo1",
        "/org/freedesktop/froyo",
        "org.freedesktop.FroyoService1",
        "Destroy",
    )
    .unwrap();
    m.append_items(&[name.into()]);
    c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;

    dbgp!("Froyodev {} destroyed", name);

    Ok(())
}

fn teardown(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodev").unwrap();

    let c = Connection::froyo_connect()?;

    let mut m = Message::new_method_call(
        "org.freedesktop.Froyo1",
        "/org/freedesktop/froyo",
        "org.freedesktop.FroyoService1",
        "Teardown",
    )
    .unwrap();
    m.append_items(&[name.into()]);
    c.send_with_reply_and_block(m, DBUS_TIMEOUT)?;

    dbgp!("Froyodev {} torn down", name);

    Ok(())
}

fn dump_meta(args: &ArgMatches) -> FroyoResult<()> {
    let name = args.value_of("froyodevname").unwrap();
    match Froyo::find(name)? {
        Some(f) => println!("{}", f.to_metadata_pretty()?),
        None => {
            return Err(FroyoError::Froyo(InternalError(
                format!("Froyodev \"{}\" not found", name).into(),
            )))
        }
    }

    Ok(())
}

fn dbus_server(_args: &ArgMatches) -> FroyoResult<()> {
    let c = Connection::froyo_connect()?;
    let froyos = Froyo::find_all()?;
    let froyos = froyos
        .into_iter()
        .map(|f| Rc::new(RefCell::new(f)))
        .collect::<Vec<_>>();
    let mut froyos = Rc::new(RefCell::new(froyos));

    // We can't change a tree from within the tree. So instead
    // register two trees, one with Create and Destroy and another for
    // querying/changing active froyodevs/
    let child_tree = dbus_api::get_child_tree(&c, &froyos.borrow())?;
    let base_tree = dbus_api::get_base_tree(&c, &mut froyos, &child_tree)?;

    // TODO: event loop needs to handle dbus and also dm events (or polling)
    // so we can extend/reshape/delay/whatever in a timely fashion
    let mut last_time = MIN_DATETIME;
    for c_item in c.iter(10000) {
        if let ConnectionItem::MethodCall(ref msg) = c_item {
            if msg.msg_type() != MessageType::MethodCall {
                continue;
            }

            if let Some(v) = base_tree.handle(msg) {
                // Probably the wisest is to ignore any send errors here -
                // maybe the remote has disconnected during our processing.
                for m in v {
                    let _ = c.send(m);
                }
            } else if let Some(v) = child_tree.borrow().handle(msg) {
                for m in v {
                    let _ = c.send(m);
                }
            }
        }

        let now = Utc::now();
        if now < last_time + Duration::seconds(30) {
            continue;
        }

        last_time = now;

        for froyo in &*froyos.borrow() {
            let mut froyo = froyo.borrow_mut();
            froyo.check_state()?;
            froyo.update_dbus()?;
            froyo.dump_status()?;
        }
    }

    Ok(())
}

fn write_err(err: FroyoError) -> FroyoResult<()> {
    let mut out = term::stderr().expect("could not get stderr");

    out.fg(term::color::RED)?;
    writeln!(out, "{}", err)?;
    out.reset()?;
    Ok(())
}

fn main() {
    let matches = App::new("froyo")
        .version(&crate_version!())
        .author("Andy Grover <andy@groveronline.com>")
        .about("Drobo + Free + YOLO")
        .arg(
            Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("Print additional output for debugging"),
        )
        .subcommand(
            SubCommand::with_name("list")
                .about("List all froyodevs")
                .arg(
                    Arg::with_name("long")
                        .short("l")
                        .long("long")
                        .help("Use a long listing format"),
                ),
        )
        .subcommand(
            SubCommand::with_name("status")
                .about("Get the status of a single froyodev")
                .arg(
                    Arg::with_name("froyodevname")
                        .required(true)
                        .help("Froyodev to get info on")
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("add")
                .about("Add one or more additional block devices to a froyodev")
                .arg(
                    Arg::with_name("force")
                        .short("f")
                        .long("force")
                        .help("Force"),
                )
                .arg(
                    Arg::with_name("froyodevname")
                        .help("Froyodev to add the device to")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("devices")
                        .help("device(s) to add")
                        .multiple(true)
                        .required(true)
                        .index(2),
                ),
        )
        .subcommand(
            SubCommand::with_name("remove")
                .about("Remove a block device from a froyodev")
                .arg(
                    Arg::with_name("wipe")
                        .long("wipe")
                        .help("No longer track this device as part of the froyodev"),
                )
                .arg(
                    Arg::with_name("froyodevname")
                        .help("Froyodev to remove the device from")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("blockdev")
                        .help("Block device to remove")
                        .required(true)
                        .index(2),
                ),
        )
        .subcommand(
            SubCommand::with_name("create")
                .about("Create a new froyodev")
                .arg(
                    Arg::with_name("force")
                        .short("f")
                        .long("force")
                        .help("Force"),
                )
                .arg(
                    Arg::with_name("froyodevname")
                        .help("Name of the new froyodev")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("devices")
                        .help("Initial block device(s) to use")
                        .multiple(true)
                        .required(true)
                        .index(2),
                ),
        )
        .subcommand(
            SubCommand::with_name("rename")
                .about("Rename a froyodev")
                .arg(
                    Arg::with_name("froyodev_old_name")
                        .help("Old name of froyodev")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("froyodev_new_name")
                        .help("New name of froyodev")
                        .required(true)
                        .index(2),
                ),
        )
        .subcommand(
            SubCommand::with_name("destroy")
                .about("Destroy a froyodev")
                .arg(
                    Arg::with_name("froyodev")
                        .help("Froyodev to destroy")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("reshape")
                .about("Manually start a reshape")
                .arg(
                    Arg::with_name("froyodev")
                        .help("Froyodev to reshape")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("teardown")
                .about("Deactivate a froyodev")
                .arg(
                    Arg::with_name("froyodev")
                        .help("Name of the froyodev")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("dev")
                .about("Developer/debug commands")
                .subcommand(
                    SubCommand::with_name("dump_meta")
                        .about("Output the JSON metadata for a froyodev")
                        .arg(
                            Arg::with_name("froyodevname")
                                .help("Name of the froyodev")
                                .required(true)
                                .index(1),
                        ),
                )
                .subcommand(SubCommand::with_name("dbus_server").about("Serve the Froyo DBus API")),
        )
        .get_matches();

    if matches.is_present("debug") {
        // must use unsafe to change a mut static, sigh
        unsafe { DEBUG = true };
    }

    let r = match matches.subcommand() {
        ("list", Some(matches)) => list(matches),
        ("status", Some(matches)) => status(matches),
        ("add", Some(matches)) => add(matches),
        ("remove", Some(matches)) => remove(matches),
        ("create", Some(matches)) => create(matches),
        ("rename", Some(matches)) => rename(matches),
        ("destroy", Some(matches)) => destroy(matches),
        ("reshape", Some(matches)) => reshape(matches),
        ("teardown", Some(matches)) => teardown(matches),
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
        if let Err(e) = write_err(r) {
            panic!("Unable to write to stderr: {}", e)
        }

        exit(1);
    }
}
