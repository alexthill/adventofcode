use std::fmt;

#[derive(Debug, Clone)]
pub enum Solution {
    None,
    U32(u32),
    U64(u64),
    Str(&'static str),
    String(String),
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U32(sol)       => write!(f, "{sol}"),
            Self::U64(sol)       => write!(f, "{sol}"),
            Self::Str(sol)       => write!(f, "{sol:?}"),
            Self::String(sol)    => write!(f, "{sol:?}"),
            Self::None           => write!(f, "No Solution"),
        }
    }
}

impl PartialEq for Solution {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::U32(a), Self::U32(b)) => a == b,
            (Self::U32(a), Self::U64(b)) => *a as u64 == *b,
            (Self::U64(a), Self::U64(b)) => a == b,
            (Self::U64(a), Self::U32(b)) => *a == *b as u64,
            (Self::Str(a), Self::Str(b)) => a == b,
            (Self::Str(a), Self::String(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::String(a), Self::Str(b)) => a == b,
            _ => false,
        }
    }
}

impl From<u32> for Solution {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}

impl From<u64> for Solution {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}

impl From<usize> for Solution {
    fn from(value: usize) -> Self {
        Self::U64(value as _)
    }
}

impl From<&'static str> for Solution {
    fn from(value: &'static str) -> Self {
        Self::Str(value)
    }
}

impl From<String> for Solution {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
