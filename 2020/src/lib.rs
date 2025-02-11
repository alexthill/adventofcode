mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;

use aoc_lib_rust::{DayNone, Year};

pub struct Year2020;

impl Year for Year2020 {

    const INPUT_DIR: &'static str = "inputs";

    type Day01 = day01::Day01;
    type Day02 = day02::Day02;
    type Day03 = day03::Day03;
    type Day04 = day04::Day04;
    type Day05 = day05::Day05;
    type Day06 = day06::Day06;
    type Day07 = day07::Day07;
    type Day08 = day08::Day08;
    type Day09 = day09::Day09;
    type Day10 = day10::Day10;
    type Day11 = day11::Day11;
    type Day12 = day12::Day12;
    type Day13 = DayNone;
    type Day14 = DayNone;
    type Day15 = DayNone;
    type Day16 = DayNone;
    type Day17 = DayNone;
    type Day18 = DayNone;
    type Day19 = DayNone;
    type Day20 = DayNone;
    type Day21 = DayNone;
    type Day22 = DayNone;
    type Day23 = DayNone;
    type Day24 = DayNone;
    type Day25 = DayNone;
}
