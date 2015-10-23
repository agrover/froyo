extern crate devicemapper;
#[macro_use]
extern crate clap;

use std::io::Result;
use std::error::Error;
use std::process::exit;

use devicemapper as devm;
use clap::{App, Arg, SubCommand, ArgMatches};

fn list(args: &ArgMatches) -> Result<()> {
    println!("hello from list()");
    Ok(())
}

fn add(args: &ArgMatches) -> Result<()> {
    println!("hello from add()");
    Ok(())
}

fn main() {

    let matches = App::new("froyo")
        .version(&crate_version!())
        .author("Andy Grover <andy@groveronline.com>")
        .about("Drobo + Free + YOLO")
        .arg(Arg::with_name("debug")
             .short("d")
             .help("Print additional output for debugging"))
        .subcommand(SubCommand::with_name("list")
                    .about("List all froyodevs")
                    .arg(Arg::with_name("long")
                         .short("l")
                         .help("Use a long listing format")))
        .subcommand(SubCommand::with_name("status")
                    .about("Get the status of a single froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .required(true)
                         .help("Froyodev to get info on")
                         .index(1)))
        .subcommand(SubCommand::with_name("add")
                    .about("Add one or more additional block devices to a froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .help("Froyodev to add the device to")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("devices")
                         .help("device to add")
                         .multiple(true)
                         .required(true)
                         .index(2))
                    )
        .subcommand(SubCommand::with_name("remove")
                    .about("Remove a block device from a froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .help("Froyodev to remove the device from")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("device")
                         .help("Block device to remove")
                         .required(true)
                         .index(2))
                    )
        .subcommand(SubCommand::with_name("create")
                    .about("Create a new froyodev")
                    .arg(Arg::with_name("froyodevname")
                         .help("Name of the new froyodev")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("devices")
                         .help("Initial block device(s) to use")
                         .multiple(true)
                         .required(true)
                         .index(2))
                    )
        .get_matches();

    let r = match matches.subcommand() {
        ("list", Some(matches)) => list(matches),
        ("add", Some(matches)) => add(matches),
        ("", None) => {
            println!("No command given, try \"help\"");
            Ok(())
        }
        _ => unreachable!(),
    };

    if let Err(r) = r {
        println!("there was an error: {}", r.description());
        exit(1);
    };
}
