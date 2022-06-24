// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::cmp::min;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fs::{read_dir, OpenOptions};
use std::io;
use std::io::{ErrorKind, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use std::str::FromStr;

use byteorder::{ByteOrder, LittleEndian};
use bytesize::ByteSize;
use chrono::{DateTime, NaiveDateTime, Utc, MIN_DATETIME};
use crc::crc32;
use devicemapper::{Device, DM};
use nix::sys::stat;
use uuid::Uuid;

use consts::*;
use dmdevice::DmDevice;
use types::{FroyoError, FroyoResult, SectorOffset, Sectors, SumSectors};
use util::blkdev_size;

pub use crate::serialize::{BlockDevSave, LinearDevSave, LinearSegment};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Mda {
    pub last_updated: DateTime<Utc>,
    length: u32,
    crc: u32,
    offset: SectorOffset,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockDev {
    pub froyodev_id: Uuid,
    pub dev: Device,
    pub id: Uuid,
    pub path: PathBuf,
    pub sectors: Sectors,
    pub mdaa: Mda,
    pub mdab: Mda,
    // Key is meta_dev dm name
    pub linear_devs: BTreeMap<String, Rc<RefCell<LinearDev>>>,
}

#[derive(Debug, Clone)]
pub enum BlockMember {
    Present(Rc<RefCell<BlockDev>>),
    Absent(BlockDevSave),
}

impl BlockMember {
    pub fn present(&self) -> Option<Rc<RefCell<BlockDev>>> {
        match *self {
            BlockMember::Present(ref x) => Some(x.clone()),
            BlockMember::Absent(_) => None,
        }
    }
}

impl BlockDev {
    pub fn new(froyodev_id: Uuid, path: &Path, force: bool) -> FroyoResult<BlockDev> {
        let pstat = stat::stat(path)?;

        if pstat.st_mode & 0x6000 != 0x6000 {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a block device", path.display()),
            )));
        }

        let dev = Device::from_str(&path.to_string_lossy())?;

        // map_err so we can improve the error message
        let mut f = OpenOptions::new().read(true).open(path).map_err(|_| {
            io::Error::new(
                ErrorKind::PermissionDenied,
                format!("Could not open {}", path.display()),
            )
        })?;

        if !force {
            let mut buf = [0u8; 4096];
            f.read_exact(&mut buf)?;

            if buf.iter().any(|x| *x != 0) {
                return Err(FroyoError::Io(io::Error::new(
                    ErrorKind::InvalidInput,
                    format!(
                        "First 4K of {} is not zeroed, need to use --force",
                        path.display()
                    ),
                )));
            }
        }

        let dev_size = blkdev_size(&f)?;
        if dev_size < MIN_DEV_SIZE {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!(
                    "{} too small, {} minimum",
                    path.display(),
                    ByteSize::b(MIN_DEV_SIZE as usize).to_string(true)
                ),
            )));
        }

        let mut bd = BlockDev {
            froyodev_id,
            id: Uuid::new_v4(),
            dev,
            path: path.to_owned(),
            sectors: Sectors(dev_size / SECTOR_SIZE),
            mdaa: Mda {
                last_updated: MIN_DATETIME,
                length: 0,
                crc: 0,
                offset: MDAA_ZONE_OFFSET,
            },
            mdab: Mda {
                last_updated: MIN_DATETIME,
                length: 0,
                crc: 0,
                offset: MDAB_ZONE_OFFSET,
            },
            linear_devs: BTreeMap::new(),
        };

        bd.write_mda_header()?;

        Ok(bd)
    }

    pub fn setup(path: &Path) -> FroyoResult<BlockDev> {
        let dev = Device::from_str(&path.to_string_lossy())?;

        // map_err so we can improve the error message
        let mut f = OpenOptions::new().read(true).open(path).map_err(|_| {
            io::Error::new(
                ErrorKind::PermissionDenied,
                format!("Could not open {}", path.display()),
            )
        })?;

        let mut buf = [0u8; HEADER_SIZE as usize];
        f.seek(SeekFrom::Start(SECTOR_SIZE))?;
        f.read_exact(&mut buf)?;

        if &buf[4..20] != FRO_MAGIC {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} is not a Froyo device", path.display()),
            )));
        }

        let crc = crc32::checksum_ieee(&buf[4..HEADER_SIZE as usize]);
        if crc != LittleEndian::read_u32(&buf[..4]) {
            dbgp!("{} Froyo header CRC failed", path.display());
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!("{} Froyo header CRC failed", path.display()),
            )));
            // TODO: Try to read end-of-disk copy
        }

        let sectors = Sectors(blkdev_size(&f)? / SECTOR_SIZE);

        let id = Uuid::from_slice(&buf[32..64]).unwrap();
        let froyodev_id = Uuid::from_slice(&buf[128..160]).unwrap();

        Ok(BlockDev {
            froyodev_id,
            id,
            dev,
            path: path.to_owned(),
            sectors,
            mdaa: Mda {
                last_updated: DateTime::<Utc>::from_utc(
                    NaiveDateTime::from_timestamp(
                        LittleEndian::read_u64(&buf[64..72]) as i64,
                        LittleEndian::read_u32(&buf[72..76]),
                    ),
                    Utc,
                ),
                length: LittleEndian::read_u32(&buf[76..80]),
                crc: LittleEndian::read_u32(&buf[80..84]),
                offset: MDAA_ZONE_OFFSET,
            },
            mdab: Mda {
                last_updated: DateTime::<Utc>::from_utc(
                    NaiveDateTime::from_timestamp(
                        LittleEndian::read_u64(&buf[96..104]) as i64,
                        LittleEndian::read_u32(&buf[104..108]),
                    ),
                    Utc,
                ),
                length: LittleEndian::read_u32(&buf[108..112]),
                crc: LittleEndian::read_u32(&buf[112..116]),
                offset: MDAB_ZONE_OFFSET,
            },
            linear_devs: BTreeMap::new(), // Not initialized until metadata is read
        })
    }

    pub fn to_save(&self) -> BlockDevSave {
        BlockDevSave {
            path: self.path.clone(),
            sectors: self.sectors,
        }
    }

    pub fn find_all() -> FroyoResult<Vec<BlockDev>> {
        Ok(read_dir("/dev")?
            .into_iter()
            .filter_map(|dir_e| {
                if let Ok(..) = dir_e {
                    Some(dir_e.unwrap().path())
                } else {
                    None
                }
            })
            .filter_map(|path| BlockDev::setup(&path).ok())
            .collect::<Vec<_>>())
    }

    fn used_areas(&self) -> Vec<(SectorOffset, Sectors)> {
        // Flag start and end mda zones as used
        let mut used = vec![
            (SectorOffset(0), MDA_ZONE_SECTORS),
            (
                SectorOffset(*self.sectors - *MDA_ZONE_SECTORS),
                MDA_ZONE_SECTORS,
            ),
        ];

        for dev in self.linear_devs.values() {
            for seg in &dev.borrow().meta_segments {
                used.push((seg.start, seg.length));
            }
            for seg in &dev.borrow().data_segments {
                used.push((seg.start, seg.length));
            }
        }
        used.sort();

        used
    }

    pub fn avail_areas(&self) -> Vec<(SectorOffset, Sectors)> {
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

    pub fn largest_avail_area(&self) -> Option<(SectorOffset, Sectors)> {
        self.avail_areas().into_iter().max_by_key(|&(_, len)| len)
    }

    // Read metadata from newest Mda
    pub fn read_mdax(&self) -> FroyoResult<Vec<u8>> {
        let younger_mda = match self.mdaa.last_updated.cmp(&self.mdab.last_updated) {
            Ordering::Less => &self.mdab,
            Ordering::Greater => &self.mdaa,
            Ordering::Equal => &self.mdab,
        };

        if younger_mda.last_updated == MIN_DATETIME {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                "Neither Mda region is in use",
            )));
        }

        let mut f = OpenOptions::new().read(true).open(&self.path)?;
        let mut buf = vec![0; younger_mda.length as usize];

        // read metadata from disk
        f.seek(SeekFrom::Start(*younger_mda.offset * SECTOR_SIZE))?;
        f.read_exact(&mut buf)?;

        if younger_mda.crc != crc32::checksum_ieee(&buf) {
            return Err(FroyoError::Io(io::Error::new(
                ErrorKind::InvalidInput,
                "Froyo Mda CRC failed",
            )));
            // TODO: Read backup copy
        }

        Ok(buf)
    }

    // Write metadata to least-recently-written Mda
    fn write_mdax(&mut self, time: &DateTime<Utc>, metadata: &[u8]) -> FroyoResult<()> {
        let older_mda = match self.mdaa.last_updated.cmp(&self.mdab.last_updated) {
            Ordering::Less => &mut self.mdaa,
            Ordering::Greater => &mut self.mdab,
            Ordering::Equal => &mut self.mdaa,
        };

        if metadata.len() as u64 > *MDAX_ZONE_SECTORS * SECTOR_SIZE {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Metadata too large for Mda, {} bytes", metadata.len()),
            )));
        }

        older_mda.crc = crc32::checksum_ieee(metadata);
        older_mda.length = metadata.len() as u32;
        older_mda.last_updated = *time;

        let mut f = OpenOptions::new().write(true).open(&self.path)?;

        // write metadata to disk
        f.seek(SeekFrom::Start(*older_mda.offset * SECTOR_SIZE))?;
        f.write_all(metadata)?;
        f.seek(SeekFrom::End(-(MDA_ZONE_SIZE as i64)))?;
        f.seek(SeekFrom::Current((*older_mda.offset * SECTOR_SIZE) as i64))?;
        f.write_all(metadata)?;
        f.flush()?;

        Ok(())
    }

    fn write_mda_header(&mut self) -> FroyoResult<()> {
        let mut buf = [0u8; HEADER_SIZE as usize];
        buf[4..20].clone_from_slice(FRO_MAGIC);
        LittleEndian::write_u64(&mut buf[20..28], *self.sectors);
        // no flags
        buf[32..64].clone_from_slice(self.id.as_bytes());

        LittleEndian::write_u64(&mut buf[64..72], self.mdaa.last_updated.timestamp() as u64);
        LittleEndian::write_u32(
            &mut buf[72..76],
            self.mdaa.last_updated.timestamp_subsec_nanos(),
        );
        LittleEndian::write_u32(&mut buf[76..80], self.mdaa.length);
        LittleEndian::write_u32(&mut buf[80..84], self.mdaa.crc);

        LittleEndian::write_u64(&mut buf[96..104], self.mdab.last_updated.timestamp() as u64);
        LittleEndian::write_u32(
            &mut buf[104..108],
            self.mdab.last_updated.timestamp_subsec_nanos(),
        );
        LittleEndian::write_u32(&mut buf[108..112], self.mdab.length);
        LittleEndian::write_u32(&mut buf[112..116], self.mdab.crc);

        buf[128..160].clone_from_slice(self.froyodev_id.as_bytes());

        // All done, calc CRC and write
        let hdr_crc = crc32::checksum_ieee(&buf[4..HEADER_SIZE as usize]);
        LittleEndian::write_u32(&mut buf[..4], hdr_crc);

        BlockDev::write_hdr_buf(&self.path, &buf)?;

        Ok(())
    }

    pub fn wipe_mda_header(&mut self) -> FroyoResult<()> {
        let buf = [0u8; HEADER_SIZE as usize];
        BlockDev::write_hdr_buf(&self.path, &buf)?;
        Ok(())
    }

    fn write_hdr_buf(path: &Path, buf: &[u8; HEADER_SIZE as usize]) -> FroyoResult<()> {
        let mut f = OpenOptions::new().write(true).open(path)?;
        let zeroed = [0u8; (SECTOR_SIZE * 8) as usize];

        // Write 4K header to head & tail. Froyo stuff goes in sector 1.
        f.write_all(&zeroed[..SECTOR_SIZE as usize])?;
        f.write_all(buf)?;
        f.write_all(&zeroed[(SECTOR_SIZE * 2) as usize..])?;
        f.seek(SeekFrom::End(-(MDA_ZONE_SIZE as i64)))?;
        f.write_all(&zeroed[..SECTOR_SIZE as usize])?;
        f.write_all(buf)?;
        f.write_all(&zeroed[(SECTOR_SIZE * 2) as usize..])?;
        f.flush()?;

        Ok(())
    }

    pub fn save_state(&mut self, time: &DateTime<Utc>, metadata: &[u8]) -> FroyoResult<()> {
        self.write_mdax(time, metadata)?;
        self.write_mda_header()?;

        Ok(())
    }

    /// Get the "x:y" device string for this blockdev
    pub fn dstr(&self) -> String {
        format!("{}:{}", self.dev.major, self.dev.minor)
    }

    // Find some sector ranges that could be allocated. If more
    // sectors are needed than our capacity, return partial results.
    pub fn get_some_space(&self, size: Sectors) -> (Sectors, Vec<(SectorOffset, Sectors)>) {
        let mut segs = Vec::new();
        let mut needed = size;

        for (start, len) in self.avail_areas() {
            if needed == Sectors(0) {
                break;
            }

            let to_use = min(needed, len);

            segs.push((start, to_use));
            needed = needed - to_use;
        }

        (size - needed, segs)
    }
}

#[derive(Debug, Clone)]
pub struct BlockDevs(pub BTreeMap<Uuid, BlockMember>);

impl BlockDevs {
    pub fn to_save(&self) -> BTreeMap<Uuid, BlockDevSave> {
        self.0
            .iter()
            .map(|(id, bd)| match *bd {
                BlockMember::Present(ref bd) => (*id, bd.borrow().to_save()),
                BlockMember::Absent(ref sbd) => (*id, sbd.clone()),
            })
            .collect()
    }

    pub fn wipe(&mut self) -> FroyoResult<()> {
        for bd in self.0.values().filter_map(|bm| bm.present()) {
            if let Err(e) = bd.borrow_mut().wipe_mda_header() {
                // keep going!
                dbgp!("Error when wiping header: {}", e);
            }
        }

        Ok(())
    }

    // Unused (non-redundant) space left on blockdevs
    pub fn unused_space(&self) -> Sectors {
        self.avail_areas()
            .iter()
            .map(|&(_, _, len)| len)
            .sum_sectors()
    }

    pub fn avail_areas(&self) -> Vec<(Rc<RefCell<BlockDev>>, SectorOffset, Sectors)> {
        self.0
            .values()
            .filter_map(|bd| bd.present())
            .flat_map(|bd| {
                let areas = bd.borrow().avail_areas();
                areas
                    .into_iter()
                    .map(|(offset, len)| (bd.clone(), offset, len))
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    pub fn get_linear_segments(
        &self,
        size: Sectors,
    ) -> Option<Vec<(Rc<RefCell<BlockDev>>, LinearSegment)>> {
        let mut needed: Sectors = size;
        let mut segs = Vec::new();

        for bd in self.0.values().filter_map(|bm| bm.present()) {
            if needed == Sectors(0) {
                break;
            }

            let (gotten, r_segs) = bd.borrow().get_some_space(needed);
            segs.extend(
                r_segs
                    .iter()
                    .map(|&(start, len)| (bd.clone(), LinearSegment::new(start, len))),
            );
            needed = needed - gotten;
        }

        match *needed {
            0 => Some(segs),
            _ => None,
        }
    }
}

impl LinearSegment {
    pub fn new(start: SectorOffset, length: Sectors) -> LinearSegment {
        LinearSegment { start, length }
    }
}

// A LinearDev contains two mappings within a single blockdev. This is
// primarily used for making RaidDevs.
#[derive(Debug, Clone)]
pub struct LinearDev {
    pub meta_dev: DmDevice,
    meta_segments: Vec<LinearSegment>,
    pub data_dev: DmDevice,
    data_segments: Vec<LinearSegment>,
    pub parent: Weak<RefCell<BlockDev>>,
}

impl PartialEq for LinearDev {
    fn eq(&self, other: &LinearDev) -> bool {
        self.meta_dev == other.meta_dev
            && self.meta_segments == other.meta_segments
            && self.data_dev == other.data_dev
            && self.data_segments == other.data_segments
            && self.parent.upgrade().unwrap() == other.parent.upgrade().unwrap()
    }
}

impl LinearDev {
    pub fn new(
        dm: &DM,
        name: &str,
        blockdev: &Rc<RefCell<BlockDev>>,
        meta_segments: &[LinearSegment],
        data_segments: &[LinearSegment],
    ) -> FroyoResult<LinearDev> {
        let ld = Self::setup(dm, name, blockdev, meta_segments, data_segments)?;
        ld.meta_dev.clear()?;
        Ok(ld)
    }

    pub fn setup(
        dm: &DM,
        name: &str,
        blockdev: &Rc<RefCell<BlockDev>>,
        meta_segments: &[LinearSegment],
        data_segments: &[LinearSegment],
    ) -> FroyoResult<LinearDev> {
        let dev = blockdev.borrow().dev;

        // meta
        let mut table = Vec::new();
        let mut offset = SectorOffset(0);
        for seg in meta_segments {
            let line = (
                *offset,
                *seg.length,
                "linear",
                format!("{}:{} {}", dev.major, dev.minor, *seg.start),
            );
            table.push(line);
            offset = offset + SectorOffset(*seg.length);
        }

        let meta_dm_name = format!("froyo-linear-meta-{}", name);
        let meta_dev = DmDevice::new(dm, &meta_dm_name, &*table)?;

        // data
        let mut table = Vec::new();
        let mut offset = SectorOffset(0);
        for seg in data_segments {
            let line = (
                *offset,
                *seg.length,
                "linear",
                format!("{}:{} {}", dev.major, dev.minor, *seg.start),
            );
            table.push(line);
            offset = offset + SectorOffset(*seg.length);
        }

        let data_dm_name = format!("froyo-linear-data-{}", name);
        let data_dev = DmDevice::new(dm, &data_dm_name, &table)?;

        Ok(LinearDev {
            meta_dev,
            meta_segments: meta_segments.to_vec(),
            data_dev,
            data_segments: data_segments.to_vec(),
            parent: Rc::downgrade(blockdev),
        })
    }

    pub fn teardown(&self, dm: &DM) -> FroyoResult<()> {
        self.meta_dev.teardown(dm)?;
        self.data_dev.teardown(dm)?;
        Ok(())
    }

    pub fn to_save(&self) -> LinearDevSave {
        let p_rc = self.parent.upgrade().unwrap();
        let parent = p_rc.borrow().id;
        LinearDevSave {
            meta_segments: self.meta_segments.clone(),
            data_segments: self.data_segments.clone(),
            parent,
        }
    }

    pub fn metadata_length(&self) -> Sectors {
        self.meta_segments.iter().map(|x| x.length).sum_sectors()
    }

    pub fn data_length(&self) -> Sectors {
        self.data_segments.iter().map(|x| x.length).sum_sectors()
    }
}
