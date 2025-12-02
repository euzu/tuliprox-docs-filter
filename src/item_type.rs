use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq, Serialize, Deserialize, Default)]
pub enum ItemType {
    #[default]
    Live,
    Video,
    Series,
}


impl ItemType {
    pub(crate) const LIVE: &'static str = "live";
    pub(crate) const VIDEO: &'static str = "video";
    pub(crate) const VOD: &'static str = "vod";
    pub(crate) const MOVIE: &'static str = "movie";
    pub(crate) const SERIES: &'static str = "series";
}

impl FromStr for ItemType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            ItemType::LIVE => Ok(ItemType::Live),
            ItemType::VOD | ItemType::VIDEO | ItemType::MOVIE => Ok(ItemType::Video),
            ItemType::SERIES => Ok(ItemType::Series),
            _ => Err(format!("Invalid PlaylistItemType: {s}")),
        }
    }
}


impl Display for ItemType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Live => Self::LIVE,
            Self::Video => Self::MOVIE,
            Self::Series => Self::SERIES,
        })
    }
}