use adventofcode_2016::Year2016;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2016, _>(env::args()).unwrap();
}
