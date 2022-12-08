use std::fmt;

pub mod utils;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;

pub const DAYS: [(fn(String) -> Solution, Solution); 8] = [
    (day01::solve, day01::SOLUTION),
    (day02::solve, day02::SOLUTION),
    (day03::solve, day03::SOLUTION),
    (day04::solve, day04::SOLUTION),
    (day05::solve, day05::SOLUTION), 
    (day06::solve, day06::SOLUTION),
    (day07::solve, day07::SOLUTION),
    (day08::solve, day08::SOLUTION),
];

#[derive(Clone, Debug)]
pub enum Solution {
    U32((u32, u32)),
    String((String, String)),
    Str((&'static str, &'static str)),
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U32((a, b))    => write!(f, "[{}, {}]", a, b),
            Self::String((a, b)) => write!(f, "[{}, {}]", a, b),
            Self::Str((a, b))    => write!(f, "[{}, {}]", a, b),
        }
    }
}

impl PartialEq for Solution {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::U32((a, b)),    Self::U32((x, y)))    => a == x && b == y,
            (Self::String((a, b)), Self::String((x, y))) => a == x && b == y,
            (Self::Str((a, b)),    Self::Str((x, y)))    => a == x && b == y,
            (Self::String((a, b)), Self::Str((x, y)))    => a == x && b == y,
            (Self::Str((a, b)),    Self::String((x, y))) => a == x && b == y,
            _ => false,
        }
    }
}

impl From<(u32, u32)> for Solution {
    fn from(value: (u32, u32)) -> Self {
        Solution::U32(value)
    }
}

impl From<(String, String)> for Solution {
    fn from(value: (String, String)) -> Self {
        Solution::String(value)
    }
}
