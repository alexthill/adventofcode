use std::env;
use std::fmt::Display;
use std::fs;
use std::num::NonZeroU8;
use std::time::{Duration, Instant};

use adventofcode_2022::*;

const SOLUTIONS: [[&'static str; 2]; 25] = [
    ["74198", "209914"], ["9241", "14610"], ["7568", "2780"], ["576", "905"], ["ZRLJGSCTR", "PRTTGRFPB"],
    ["", ""], ["", ""], ["", ""], ["", ""], ["", ""],
    ["", ""], ["", ""], ["", ""], ["", ""], ["", ""],
    ["", ""], ["", ""], ["", ""], ["", ""], ["", ""],
    ["", ""], ["", ""], ["", ""], ["", ""], ["", ""],
];

fn main() {
    let args: Vec<String> = env::args().collect();
    let day = args.get(1).expect("expected one argument");
    
    if day == "all" {
        let start = Instant::now();
        let comp_time: Duration = (1..=5).map(solve_day).sum();
        let total_time = start.elapsed();
        println!("========================================");
        println!("took {:?} with i/o and {:?} without", total_time, comp_time);
    } else {
        let day = day.parse::<NonZeroU8>()
            .expect("argument must be either `all` or a positive number")
            .get();
        solve_day(day);
    }   
}

fn solve_day(day: u8) -> Duration {
    let input = fs::read_to_string(format!("inputs/{:0>2}.txt", day)).unwrap();
    let start = Instant::now();

    match day {
        1 => print_res(day, &start, day01::solve(input)),
        2 => print_res(day, &start, day02::solve(input)),
        3 => print_res(day, &start, day03::solve(input)),
        4 => print_res(day, &start, day04::solve(input)),
        5 => print_res(day, &start, day05::solve(input)),
        other => panic!("day {} does not exists", other),
    }
}

fn print_res<T: Display, U: Display>(day: u8, start: &Instant, res: (T, U)) -> Duration {
    let time = start.elapsed();
    let res = [res.0.to_string(), res.1.to_string()];
    let expected = SOLUTIONS[day as usize - 1];
    println!("solutions for Day {} ({:?}):", day, time);
    println!(" - part 1: {}", res[0]);
    println!(" - part 2: {}", res[1]);
    if res != expected {
        println!(" - solutions are wrong, schould be: {:?}", expected);
    }
    time
}
