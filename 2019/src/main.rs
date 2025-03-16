use adventofcode_2019::Year2019;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2019, _>(env::args()).unwrap();
}
