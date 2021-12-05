use serde::{Deserialize, Serialize};
use uuid::Uuid;

use std::collections::BTreeMap;
use std::path::PathBuf;

use types::{DataBlocks, SectorOffset, Sectors};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockDevSave {
    pub path: PathBuf,
    pub sectors: Sectors,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct LinearSegment {
    pub start: SectorOffset,
    pub length: Sectors,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LinearDevSave {
    pub meta_segments: Vec<LinearSegment>,
    pub data_segments: Vec<LinearSegment>,
    pub parent: Uuid,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FroyoSave {
    pub name: String,
    pub id: Uuid,
    pub block_devs: BTreeMap<Uuid, BlockDevSave>,
    pub raid_devs: BTreeMap<Uuid, RaidDevSave>,
    pub thin_pool_dev: ThinPoolDevSave,
    pub thin_devs: Vec<ThinDevSave>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temp_dev: Option<TempDevSave>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TempDevSegmentSave {
    pub parent: Uuid,
    pub start: SectorOffset,
    pub length: Sectors,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TempDevSave {
    pub id: Uuid,
    pub segments: Vec<TempDevSegmentSave>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaidDevSave {
    pub stripe_sectors: Sectors,
    pub region_sectors: Sectors,
    pub length: Sectors,
    pub member_count: usize,
    pub members: BTreeMap<usize, LinearDevSave>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaidSegmentSave {
    pub start: SectorOffset,
    pub length: Sectors,
    pub parent: Uuid, // RaidDev id
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaidLinearDevSave {
    pub id: Uuid,
    pub segments: Vec<RaidSegmentSave>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThinPoolDevSave {
    pub data_block_size: Sectors,
    pub low_water_blocks: DataBlocks,
    pub meta_dev: RaidLinearDevSave,
    pub data_dev: RaidLinearDevSave,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThinDevSave {
    pub name: String,
    pub thin_number: u32,
    pub size: Sectors,
}
