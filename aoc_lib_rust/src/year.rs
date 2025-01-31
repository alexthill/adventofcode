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

    const INPUT_DIR: &'static str;

    fn solve(day: Option<&str>) -> bool {
        let Some(day) = day else {
            eprintln!("Please provide the day you want to run or 'all'.");
            return false;
        };

        if day == "all" {
            Self::solve_all().unwrap();
            return true;
        }
        if let Ok(day) = day.parse::<u8>() {
            let found_day = Self::solve_specific_day(day).unwrap();
            if !found_day {
                eprintln!("Day {day} is not a valid day.");
            }
            return found_day;
        }

        eprintln!("Day {day} is not a valid day.");
        return false;
    }

    fn solve_all() -> Result<(), io::Error> {
        for i in 1..=25 {
            Self::solve_specific_day(i)?;
        }
        Ok(())
    }

    fn solve_day<const N: u8, D: Day>(file_name: &str) -> Result<(), io::Error> {
        println!("Solving Day {N:02}:");

        if D::DAY_NONE {
            println!("\tnot solved");
            return Ok(());
        }

        if let Err((example, sol)) = D::check_examples() {
            println!("\t❌ failed example: expected {:?}, found {:?}", example.solution, sol);
            println!("\tinput:\n{}", example.input);
            return Ok(());
        }

        let path = format!("{}/{file_name}", Self::INPUT_DIR);
        println!("\treading {path}");
        let input = fs::read_to_string(path)?;
        let bytes = input.as_bytes();
        let input = if !bytes.is_empty() && bytes[bytes.len() - 1] == b'\n' {
            &input[..input.len() - 1]
        } else {
            &input[..]
        };

        let start = Instant::now();
        let sol = D::solve(&input);
        let time = start.elapsed();

        println!("\ttime: {time:?}");
        println!("\tsolution: [{}, {}]", sol[0], sol[1]);
        if sol != [D::PART1, D::PART2] {
            println!("\t❌ expected: [{}, {}]", D::PART1, D::PART2)
        }

        Ok(())
    }

    fn solve_specific_day(day: u8) -> Result<bool, io::Error> {
        match day {
            1 => Self::solve_day::<1, Self::Day01>("day01.txt")?,
            2 => Self::solve_day::<2, Self::Day02>("day02.txt")?,
            3 => Self::solve_day::<3, Self::Day03>("day03.txt")?,
            4 => Self::solve_day::<4, Self::Day04>("day04.txt")?,
            5 => Self::solve_day::<5, Self::Day05>("day05.txt")?,
            6 => Self::solve_day::<6, Self::Day06>("day06.txt")?,
            7 => Self::solve_day::<7, Self::Day07>("day07.txt")?,
            8 => Self::solve_day::<8, Self::Day08>("day08.txt")?,
            9 => Self::solve_day::<9, Self::Day09>("day09.txt")?,
            10 => Self::solve_day::<10, Self::Day10>("day10.txt")?,
            11 => Self::solve_day::<11, Self::Day11>("day11.txt")?,
            12 => Self::solve_day::<12, Self::Day12>("day12.txt")?,
            13 => Self::solve_day::<13, Self::Day13>("day13.txt")?,
            14 => Self::solve_day::<14, Self::Day14>("day14.txt")?,
            15 => Self::solve_day::<15, Self::Day15>("day15.txt")?,
            16 => Self::solve_day::<16, Self::Day16>("day16.txt")?,
            17 => Self::solve_day::<17, Self::Day17>("day17.txt")?,
            18 => Self::solve_day::<18, Self::Day18>("day18.txt")?,
            19 => Self::solve_day::<19, Self::Day19>("day19.txt")?,
            20 => Self::solve_day::<20, Self::Day20>("day20.txt")?,
            21 => Self::solve_day::<21, Self::Day21>("day21.txt")?,
            22 => Self::solve_day::<22, Self::Day22>("day22.txt")?,
            23 => Self::solve_day::<23, Self::Day23>("day23.txt")?,
            24 => Self::solve_day::<24, Self::Day24>("day24.txt")?,
            25 => Self::solve_day::<25, Self::Day25>("day25.txt")?,
            _ => return Ok(false),
        };
        return Ok(true);
    }
}
