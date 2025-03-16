use adventofcode_2020::Year2020;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2020, _>(env::args()).unwrap();
}
