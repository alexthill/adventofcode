use adventofcode_2015::Year2015;
use aoc_lib_rust::cli_print;
use std::env;

fn main() {
    cli_print::<Year2015, _>(env::args()).unwrap();
}
