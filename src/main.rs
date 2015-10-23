extern crate devicemapper;
#[macro_use]
extern crate clap;

use std::io::Result;

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
        .subcommand(SubCommand::with_name("list")
                    .about("lists stuff")
                    .arg(Arg::with_name("poolname")
                         .help("optional pool to list")
                         .index(1)))
        .subcommand(SubCommand::with_name("add")
                    .about("add a device to a pool")
                    .arg(Arg::with_name("poolname")
                         .help("pool to list")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("device")
                         .help("device to add")
                         .required(true)
                         .index(2))
                    )
        .get_matches();

    match matches.subcommand() {
        ("list", Some(matches)) => list(matches),
        ("add", Some(matches)) => add(matches),
        _ => unreachable!(),
    };
}
