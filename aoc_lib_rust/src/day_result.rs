use crate::Solution;
use std::time::Duration;

pub struct DayResult {
    pub is_example: bool,
    pub found: [Solution; 2],
    pub expected: [Solution; 2],
    pub input: String,
    pub time: Duration,
}
