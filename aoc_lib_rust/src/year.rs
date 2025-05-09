use crate::Day;
use crate::cli::Flags;
use crate::day_result::DayResult;

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
        flags: Flags,
        out: &mut O,
    ) -> Result<Option<DayResult>, io::Error> {
        if D::DAY_NONE {
            return Ok(None);
        }

        let start = Instant::now();
        if let Err((example, found)) = D::check_examples() {
            let time = start.elapsed();
            return Ok(Some(DayResult {
                is_example: true,
                found,
                expected: example.solution.clone(),
                input: example.input.to_owned(),
                time,
            }));
        }

        let path = format!("{}/day{:02?}.txt", Self::INPUT_DIR, flags.day);
        if !flags.benchmark && !flags.sort {
            out(format!("\treading {path}"));
        }
        let input = fs::read_to_string(path)?;
        let bytes = input.as_bytes();
        let trimmed_input = if !bytes.is_empty() && bytes[bytes.len() - 1] == b'\n' {
            &input[..input.len() - 1]
        } else {
            &input[..]
        };

        let start = Instant::now();
        let found = D::solve2(trimmed_input, false);
        let time = start.elapsed();

        Ok(Some(DayResult {
            is_example: false,
            found,
            expected: [D::PART1, D::PART2],
            input,
            time,
        }))
    }

    fn solve_specific_day<O: FnMut(String)>(
        flags: Flags,
        out: &mut O,
    ) -> Result<Option<DayResult>, io::Error> {
        if !flags.benchmark && !flags.sort {
            out(format!("Solving Day {:02}:", flags.day));
        }

        let res = match flags.day {
            1 => Self::solve_day::<1, Self::Day01, O>(flags, out)?,
            2 => Self::solve_day::<2, Self::Day02, O>(flags, out)?,
            3 => Self::solve_day::<3, Self::Day03, O>(flags, out)?,
            4 => Self::solve_day::<4, Self::Day04, O>(flags, out)?,
            5 => Self::solve_day::<5, Self::Day05, O>(flags, out)?,
            6 => Self::solve_day::<6, Self::Day06, O>(flags, out)?,
            7 => Self::solve_day::<7, Self::Day07, O>(flags, out)?,
            8 => Self::solve_day::<8, Self::Day08, O>(flags, out)?,
            9 => Self::solve_day::<9, Self::Day09, O>(flags, out)?,
            10 => Self::solve_day::<10, Self::Day10, O>(flags, out)?,
            11 => Self::solve_day::<11, Self::Day11, O>(flags, out)?,
            12 => Self::solve_day::<12, Self::Day12, O>(flags, out)?,
            13 => Self::solve_day::<13, Self::Day13, O>(flags, out)?,
            14 => Self::solve_day::<14, Self::Day14, O>(flags, out)?,
            15 => Self::solve_day::<15, Self::Day15, O>(flags, out)?,
            16 => Self::solve_day::<16, Self::Day16, O>(flags, out)?,
            17 => Self::solve_day::<17, Self::Day17, O>(flags, out)?,
            18 => Self::solve_day::<18, Self::Day18, O>(flags, out)?,
            19 => Self::solve_day::<19, Self::Day19, O>(flags, out)?,
            20 => Self::solve_day::<20, Self::Day20, O>(flags, out)?,
            21 => Self::solve_day::<21, Self::Day21, O>(flags, out)?,
            22 => Self::solve_day::<22, Self::Day22, O>(flags, out)?,
            23 => Self::solve_day::<23, Self::Day23, O>(flags, out)?,
            24 => Self::solve_day::<24, Self::Day24, O>(flags, out)?,
            25 => Self::solve_day::<25, Self::Day25, O>(flags, out)?,
            _ => unreachable!(),
        };
        if flags.benchmark  || flags.sort {
            return Ok(res);
        }
        if let Some(ref res) = res {
            out(format!("\ttime: {:?}", res.time));
            out(format!("\tsolution: [{}, {}]", res.found[0], res.found[1]));
            if res.found != res.expected {
                if res.is_example {
                    out(format!("\t❌ failed example: expected: {:?}", res.expected))
                } else {
                    out(format!("\t❌ expected: {:?}", res.expected))
                }
            }
        } else {
            out("\tnot solved".to_owned());
        }
        Ok(res)
    }
}

pub struct MockYear;

impl Year for MockYear {
    const YEAR: u16 = 2015;
    const INPUT_DIR: &'static str = "mock_inputs";

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
