use super::{Day};

use std::fs;
use std::io;
use std::time::Instant;

pub trait Year {
    type Day01: Day;
    type Day02: Day;
    type Day03: Day;
    type Day04: Day;
    type Day05: Day;
    type Day06: Day;
    type Day07: Day;
    type Day08: Day;
    type Day09: Day;
    type Day10: Day;
    type Day11: Day;
    type Day12: Day;
    type Day13: Day;
    type Day14: Day;
    type Day15: Day;
    type Day16: Day;
    type Day17: Day;
    type Day18: Day;
    type Day19: Day;
    type Day20: Day;
    type Day21: Day;
    type Day22: Day;
    type Day23: Day;
    type Day24: Day;
    type Day25: Day;

    const YEAR: u16;
    const INPUT_DIR: &'static str;

    fn solve_day<const N: u8, D: Day, O: FnMut(String)>(
        file_name: &str,
        mut out: O,
    ) -> Result<(), io::Error> {
        out(format!("Solving Day {N:02}:"));

        if D::DAY_NONE {
            out(format!("\tnot solved"));
            return Ok(());
        }

        if let Err((example, sol)) = D::check_examples() {
            out(format!("\t❌ failed example: expected {:?}, found {:?}", example.solution, sol));
            out(format!("\tinput:\n{}", example.input));
            return Ok(());
        }

        let path = format!("{}/{file_name}", Self::INPUT_DIR);
        out(format!("\treading {path}"));
        let input = fs::read_to_string(path)?;
        let bytes = input.as_bytes();
        let input = if !bytes.is_empty() && bytes[bytes.len() - 1] == b'\n' {
            &input[..input.len() - 1]
        } else {
            &input[..]
        };

        let start = Instant::now();
        let sol = D::solve2(&input, false);
        let time = start.elapsed();

        out(format!("\ttime: {time:?}"));
        out(format!("\tsolution: [{}, {}]", sol[0], sol[1]));
        if sol != [D::PART1, D::PART2] {
            out(format!("\t❌ expected: [{}, {}]", D::PART1, D::PART2))
        }

        Ok(())
    }

    fn solve_specific_day<O: FnMut(String)>(day: u8, out: O) -> Result<(), io::Error> {
        match day {
            1 => Self::solve_day::<1, Self::Day01, O>("day01.txt", out)?,
            2 => Self::solve_day::<2, Self::Day02, O>("day02.txt", out)?,
            3 => Self::solve_day::<3, Self::Day03, O>("day03.txt", out)?,
            4 => Self::solve_day::<4, Self::Day04, O>("day04.txt", out)?,
            5 => Self::solve_day::<5, Self::Day05, O>("day05.txt", out)?,
            6 => Self::solve_day::<6, Self::Day06, O>("day06.txt", out)?,
            7 => Self::solve_day::<7, Self::Day07, O>("day07.txt", out)?,
            8 => Self::solve_day::<8, Self::Day08, O>("day08.txt", out)?,
            9 => Self::solve_day::<9, Self::Day09, O>("day09.txt", out)?,
            10 => Self::solve_day::<10, Self::Day10, O>("day10.txt", out)?,
            11 => Self::solve_day::<11, Self::Day11, O>("day11.txt", out)?,
            12 => Self::solve_day::<12, Self::Day12, O>("day12.txt", out)?,
            13 => Self::solve_day::<13, Self::Day13, O>("day13.txt", out)?,
            14 => Self::solve_day::<14, Self::Day14, O>("day14.txt", out)?,
            15 => Self::solve_day::<15, Self::Day15, O>("day15.txt", out)?,
            16 => Self::solve_day::<16, Self::Day16, O>("day16.txt", out)?,
            17 => Self::solve_day::<17, Self::Day17, O>("day17.txt", out)?,
            18 => Self::solve_day::<18, Self::Day18, O>("day18.txt", out)?,
            19 => Self::solve_day::<19, Self::Day19, O>("day19.txt", out)?,
            20 => Self::solve_day::<20, Self::Day20, O>("day20.txt", out)?,
            21 => Self::solve_day::<21, Self::Day21, O>("day21.txt", out)?,
            22 => Self::solve_day::<22, Self::Day22, O>("day22.txt", out)?,
            23 => Self::solve_day::<23, Self::Day23, O>("day23.txt", out)?,
            24 => Self::solve_day::<24, Self::Day24, O>("day24.txt", out)?,
            25 => Self::solve_day::<25, Self::Day25, O>("day25.txt", out)?,
            _ => {}
        };
        return Ok(());
    }
}

pub struct MockYear;

impl Year for MockYear {
    const YEAR: u16 = 2015;
    const INPUT_DIR: &str = "mock_inputs";

    type Day01 = crate::DayNone;
    type Day02 = crate::DayNone;
    type Day03 = crate::DayNone;
    type Day04 = crate::DayNone;
    type Day05 = crate::DayNone;
    type Day06 = crate::DayNone;
    type Day07 = crate::DayNone;
    type Day08 = crate::DayNone;
    type Day09 = crate::DayNone;
    type Day10 = crate::DayNone;
    type Day11 = crate::DayNone;
    type Day12 = crate::DayNone;
    type Day13 = crate::DayNone;
    type Day14 = crate::DayNone;
    type Day15 = crate::DayNone;
    type Day16 = crate::DayNone;
    type Day17 = crate::DayNone;
    type Day18 = crate::DayNone;
    type Day19 = crate::DayNone;
    type Day20 = crate::DayNone;
    type Day21 = crate::DayNone;
    type Day22 = crate::DayNone;
    type Day23 = crate::DayNone;
    type Day24 = crate::DayNone;
    type Day25 = crate::DayNone;
}
