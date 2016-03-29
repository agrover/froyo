// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use types::{Sectors, SectorOffset};

pub const REDUNDANCY: usize = 1;
pub const MIN_BLK_DEVS: usize = 2;

const MEGA: u64 = 1024 * 1024;
const GIGA: u64 = MEGA * 1024;
const TERA: u64 = GIGA * 1024;

// Before increasing, either check if md-raid is good for wider, or
// change zone creation to limit width
pub const MAX_BLK_DEVS: usize = 8;

pub const SECTOR_SIZE: u64 = 512;
pub const HEADER_SIZE: u64 = 512;
pub const MDA_ZONE_SIZE: u64 = MEGA;
pub const MDA_ZONE_SECTORS: Sectors = Sectors::new(MDA_ZONE_SIZE / SECTOR_SIZE);
pub const MDAX_ZONE_SECTORS: Sectors = Sectors::new(1020);
pub const MDAA_ZONE_OFFSET: SectorOffset = SectorOffset::new(8);
pub const MDAB_ZONE_OFFSET: SectorOffset = SectorOffset::new(1028);

pub const FRO_MAGIC: &'static [u8] = b"!IamFroy0\x86\xffGO\x02^\x41";
pub const STRIPE_SECTORS: Sectors = Sectors::new(2048);

// No devs smaller than around a gig
const MIN_DATA_ZONE_SIZE: u64 = GIGA;
pub const MIN_DATA_ZONE_SECTORS: Sectors = Sectors::new(MIN_DATA_ZONE_SIZE / SECTOR_SIZE);
pub const MIN_DEV_SIZE: u64 = MIN_DATA_ZONE_SIZE + (2 * MDA_ZONE_SIZE);

// But also no larger than 1 TiB
const MAX_DATA_ZONE_SIZE: u64 = TERA;
pub const MAX_DATA_ZONE_SECTORS: Sectors = Sectors::new(MAX_DATA_ZONE_SIZE / SECTOR_SIZE);

//const MIN_DEV_SECTORS: u64 = MIN_DEV_SIZE / SECTOR_SIZE;

pub const MAX_REGIONS: u64 = (2 * MEGA);
const DEFAULT_REGION_SIZE: u64 = (4 * MEGA);
pub const DEFAULT_REGION_SECTORS: Sectors = Sectors::new(DEFAULT_REGION_SIZE / SECTOR_SIZE);

// Try to create this many raids in a Froyodev. Too many is wasteful
// but too few limits reshape options (at least until dn-raid supports
// reshape)
pub const IDEAL_RAID_COUNT: usize = 10;
