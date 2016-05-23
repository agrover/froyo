// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;
use std::rc::Rc;
use std::cell::RefCell;

use uuid::Uuid;
use devicemapper::DM;

use froyo::FroyoSave;
use types::{Sectors, SectorOffset, FroyoResult, FroyoError, InternalError};
use consts::*;
use blockdev::{LinearSegment, BlockDev, BlockDevs};
use dmdevice::DmDevice;
use raid::{RaidDev, RaidLinearDev};

#[derive(Debug, Clone)]
pub struct MirrorDev {
    pub mirror: DmDevice,
    pub src: Rc<RefCell<TempDev>>,
    pub dest: Rc<RefCell<TempDev>>,
    // info on raidlineardev in thinpool so we can restore table when done
    pub linear_dev: Rc<RefCell<RaidLinearDev>>,
    pub linear_dev_idxs: Vec<usize>,
}

impl MirrorDev {
    pub fn new(
        dm: &DM,
        name: &str,
        src: Rc<RefCell<TempDev>>,
        dest: Rc<RefCell<TempDev>>,
        length: Sectors,
        linear_dev: Rc<RefCell<RaidLinearDev>>,
        linear_dev_idxs: &[usize])
               -> FroyoResult<MirrorDev> {
        let table = (0, *length, "raid",
                     format!("raid1 1 {} 2 - {} - {}",
                             *STRIPE_SECTORS,
                             src.borrow().dmdev.dstr(),
                             dest.borrow().dmdev.dstr()));
        let dm_name = format!("froyo-copymirror-{}", name);
        let mirror_dev = try!(DmDevice::new(dm, &dm_name, &[table]));

        Ok(MirrorDev {
            mirror: mirror_dev,
            src: src,
            dest: dest,
            linear_dev: linear_dev,
            linear_dev_idxs: linear_dev_idxs.to_vec(),
        })
    }

    pub fn teardown(&self, dm: &DM) -> FroyoResult<()> {
        try!(self.mirror.teardown(dm));
        try!(self.src.borrow().teardown(dm));
        try!(self.dest.borrow().teardown(dm));
        Ok(())
    }

    pub fn is_syncing(&self, dm: &DM) -> FroyoResult<bool> {
        let mut status = try!(self.mirror.table_status(dm));

        // See kernel's dm-raid.txt "Status Output"
        let status_line = status.pop().unwrap().3;
        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        if status_vals.len() < 5 {
            return Err(FroyoError::Io(io::Error::new(
                io::ErrorKind::InvalidData,
                "Kernel returned too few values from raid status")))
        }

        dbgp!("status line {}", status_line);
        dbgp!("status {}", status_vals[2]);
        dbgp!("action {}", status_vals[4]);

         match status_vals[4] {
             "idle" => Ok(false),
             "resync" => Ok(true),
             action => Err(FroyoError::Froyo(InternalError(
                 format!("Unexpected action: {}", action).into()))),
         }
    }
}

// Our TempDev's segments may either be on top of a RaidDev (e.g. if
// we're using the MirrorDev to copy to/from two raids) or it may be
// on top of a BlockDev, if we've resorted to using non-redundant
// scratch space.
#[derive(Debug, Clone)]
pub enum TempLayer {
    Block(Rc<RefCell<BlockDev>>),
    Raid(Rc<RefCell<RaidDev>>),
}

impl TempLayer {
    pub fn dstr(&self) -> String {
        match *self {
            TempLayer::Block(ref bd) => bd.borrow().dev.dstr(),
            TempLayer::Raid(ref rd) => rd.borrow().dev.dstr(),
        }
    }

    pub fn block(&self) -> Rc<RefCell<BlockDev>> {
        match *self {
            TempLayer::Block(ref bd) => bd.clone(),
            _ => panic!("should never happen"),
        }
    }

    pub fn raid(&self) -> Rc<RefCell<RaidDev>> {
        match *self {
            TempLayer::Raid(ref rd) => rd.clone(),
            _ => panic!("should never happen"),
        }
    }

    pub fn id(&self) -> String {
        match *self {
            TempLayer::Block(ref bd) => bd.borrow().id.to_owned(),
            TempLayer::Raid(ref rd) => rd.borrow().id.to_owned(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TempDevSegmentSave {
    pub parent: String,
    pub start: SectorOffset,
    pub length: Sectors,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TempDevSave {
    pub id: String,
    pub segments: Vec<TempDevSegmentSave>,
}

#[derive(Debug, Clone)]
pub struct TempDev {
    pub id: String,
    pub dmdev: DmDevice,
    pub segments: Vec<(TempLayer, LinearSegment)>,
}

// A linear device that can span multiple blockdevs.

impl TempDev {
    pub fn new(
        dm: &DM,
        name: &str,
        segments: &[(TempLayer, LinearSegment)])
        -> FroyoResult<TempDev> {

        let mut table = Vec::new();
        let mut offset = SectorOffset(0);
        for &(ref dev, seg) in segments {
            let line = (*offset, *seg.length, "linear",
                        format!("{} {}", dev.dstr(), *seg.start));
            table.push(line);
            offset = offset + SectorOffset(*seg.length);
        }

        let id = Uuid::new_v4().to_simple_string();
        let dm_name = format!("froyo-linear-temp-{}-{}", name, id);
        let dmdev = try!(DmDevice::new(dm, &dm_name, &table));

        Ok(TempDev {
            id: id,
            dmdev: dmdev,
            segments: segments.to_vec(),
        })
    }

    pub fn setup(
        dm: &DM,
        froyo_save: &FroyoSave,
        block_devs: &BlockDevs)
        -> FroyoResult<Option<TempDev>> {

        match froyo_save.temp_dev {
            None => Ok(None),
            Some(ref td) => {
                let mut td_segs = Vec::new();
                for seg in &td.segments {
                    if let Some(bd) = block_devs.0.get(&seg.parent)
                        .and_then(|bm| bm.present()) {
                        td_segs.push((TempLayer::Block(bd.clone()),
                                      LinearSegment::new(seg.start, seg.length)));
                    }
                }

                let td = try!(TempDev::new(dm, &froyo_save.id, &td_segs));
                Ok(Some(td))
            }
        }
    }


    pub fn dstr(&self) -> String {
        self.dmdev.dstr()
    }

    pub fn teardown(&self, dm: &DM) -> FroyoResult<()> {
        self.dmdev.teardown(dm)
    }

    pub fn length(&self) -> Sectors {
        self.segments.iter().map(|&(_, x)| x.length).sum()
    }

    pub fn to_save(&self) -> TempDevSave {
        TempDevSave {
            id: self.id.to_owned(),
            segments: self.segments.iter()
                .map(|&(ref tl, ls)| TempDevSegmentSave {
                    parent: tl.id(),
                    start: ls.start,
                    length: ls.length,
                })
                .collect(),
        }
    }
}
