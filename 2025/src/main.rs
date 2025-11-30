use adventofcode_2025::Year2025;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2025, _>(env::args()).unwrap();
}
