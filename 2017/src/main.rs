use adventofcode_2017::Year2017;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2017, _>(env::args()).unwrap();
}
