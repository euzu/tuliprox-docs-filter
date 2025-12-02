use std::fmt::Display;
use std::str::FromStr;
use enum_iterator::Sequence;
use serde::{Deserialize, Deserializer};

#[derive(Debug, Copy, Clone, serde::Serialize, Sequence, Eq, PartialEq)]
pub enum ItemField {
    Group,
    Name,
    Title,
    Url,
    Input,
    Type,
    Caption,
}

impl ItemField {
    const GROUP: &'static str = "Group";
    const NAME: &'static str = "Name";
    const TITLE: &'static str = "Title";
    const URL: &'static str = "Url";
    const INPUT: &'static str = "Input";
    const TYPE: &'static str = "Type";
    const CAPTION: &'static str = "Caption";

    pub fn as_str(&self) -> &'static str {
        match *self {
            Self::Group => Self::GROUP,
            Self::Name => Self::NAME,
            Self::Title => Self::TITLE,
            Self::Url => Self::URL,
            Self::Input => Self::INPUT,
            Self::Type => Self::TYPE,
            Self::Caption => Self::CAPTION,
        }
    }
}

impl Display for ItemField {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match *self {
            Self::Group => Self::GROUP,
            Self::Name => Self::NAME,
            Self::Title => Self::TITLE,
            Self::Url => Self::URL,
            Self::Input => Self::INPUT,
            Self::Type => Self::TYPE,
            Self::Caption => Self::CAPTION,
        })
    }
}

impl FromStr for ItemField {
    type Err = std::fmt::Error;

    fn from_str(s: &str) -> Result<Self, std::fmt::Error> {
        if s.eq_ignore_ascii_case(Self::GROUP) {
            Ok(Self::Group)
        } else if s.eq_ignore_ascii_case(Self::NAME) {
            Ok(Self::Name)
        } else if s.eq_ignore_ascii_case(Self::TITLE) {
            Ok(Self::Title)
        } else if s.eq_ignore_ascii_case(Self::CAPTION) {
            Ok(Self::Caption)
        } else if s.eq_ignore_ascii_case(Self::URL) {
            Ok(Self::Url)
        } else if s.eq_ignore_ascii_case(Self::INPUT) {
            Ok(Self::Input)
        } else if s.eq_ignore_ascii_case(Self::TYPE) {
            Ok(Self::Type)
        } else {
            Err(std::fmt::Error{} /*::custom(format!("Unknown InputType: {s}")*/)
        }
    }
}

impl<'de> Deserialize<'de> for ItemField {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        ItemField::from_str(&s).map_err(serde::de::Error::custom)
    }
}