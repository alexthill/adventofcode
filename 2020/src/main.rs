mod year2020;

use aoc_lib_rust::Year;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    year2020::Year2020::solve(args.get(1).map(|x| x.as_str()));
}
