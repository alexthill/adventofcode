mod day;
mod solution;
mod year;

pub use day::{Day, DayNone, Example};
pub use solution::Solution;
pub use year::Year;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
