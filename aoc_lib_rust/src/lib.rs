mod cli;
mod day;
mod day_result;
mod solution;
pub mod utils;
mod year;

pub use cli::{cli, cli_print};
pub use day::{Day, DayNone, Example};
pub use solution::Solution;
pub use year::Year;
