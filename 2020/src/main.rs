use adventofcode_2020::Year2020;
use aoc_lib_rust::Year;
use std::env;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    Year2020::solve(&args);
}
