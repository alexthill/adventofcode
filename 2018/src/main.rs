use adventofcode_2018::Year2018;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2018, _>(env::args()).unwrap();
}
