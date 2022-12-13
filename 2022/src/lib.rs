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
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;

pub const DAYS: [(fn(String) -> Solution, Solution); 13] = [
    (day01::solve, day01::SOLUTION),
    (day02::solve, day02::SOLUTION),
    (day03::solve, day03::SOLUTION),
    (day04::solve, day04::SOLUTION),
    (day05::solve, day05::SOLUTION), 
    (day06::solve, day06::SOLUTION),
    (day07::solve, day07::SOLUTION),
    (day08::solve, day08::SOLUTION),
    (day09::solve, day09::SOLUTION),
    (day10::solve, day10::SOLUTION),
    (day11::solve, day11::SOLUTION),
    (day12::solve, day12::SOLUTION),
    (day13::solve, day13::SOLUTION),
];

#[derive(Clone, Debug)]
pub enum Solution {
    U32((u32, u32)),
    U64((u64, u64)),
    String((String, String)),
    Str((&'static str, &'static str)),
    I32String((i32, String)),
    I32Str((i32, &'static str)),
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U32((a, b))       => write!(f, "[{}, {}]", a, b),
            Self::U64((a, b))       => write!(f, "[{}, {}]", a, b),
            Self::String((a, b))    => write!(f, "[{}, {}]", a, b),
            Self::Str((a, b))       => write!(f, "[{}, {}]", a, b),
            Self::I32String((a, b)) => write!(f, "[{}, {}]", a, b),
            Self::I32Str((a, b))    => write!(f, "[{}, {}]", a, b),
        }
    }
}

impl PartialEq for Solution {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::U32((a, b)),       Self::U32((x, y)))       => a == x && b == y,
            (Self::U64((a, b)),       Self::U64((x, y)))       => a == x && b == y,
            (Self::String((a, b)),    Self::String((x, y)))    => a == x && b == y,
            (Self::Str((a, b)),       Self::Str((x, y)))       => a == x && b == y,
            (Self::String((a, b)),    Self::Str((x, y)))       => a == x && b == y,
            (Self::Str((a, b)),       Self::String((x, y)))    => a == x && b == y,
            (Self::I32String((a, b)), Self::I32String((x, y))) => a == x && b == y,
            (Self::I32String((a, b)), Self::I32Str((x, y)))    => a == x && b == y,
            (Self::I32Str((a, b)),    Self::I32String((x, y))) => a == x && b == y,
            _ => false,
        }
    }
}

impl From<(u32, u32)> for Solution {
    fn from(value: (u32, u32)) -> Self {
        Solution::U32(value)
    }
}

impl From<(u64, u64)> for Solution {
    fn from(value: (u64, u64)) -> Self {
        Solution::U64(value)
    }
}

impl From<(String, String)> for Solution {
    fn from(value: (String, String)) -> Self {
        Solution::String(value)
    }
}

impl From<(i32, String)> for Solution {
    fn from(value: (i32, String)) -> Self {
        Solution::I32String(value)
    }
}
