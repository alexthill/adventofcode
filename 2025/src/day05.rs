use aoc_lib_rust::{Day, Example, Solution};
use std::ops::RangeInclusive;

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::U32(739);
    const PART2: Solution = Solution::U64(344486348901788);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.lines();
        let mut ranges: Vec<RangeInclusive<u64>> = Vec::new();
        while let Some(line) = lines.next() && !line.is_empty() {
            let (a, b) = line.split_once('-').unwrap();
            let new = a.parse::<u64>().unwrap()..=b.parse::<u64>().unwrap();
            let mut new_ranges = vec![new];
            for range in ranges.iter_mut() {
                let old_new_ranges = std::mem::take(&mut new_ranges);
                for new in old_new_ranges {
                    if range.start() <= new.start() && range.end() >= new.end() {
                        // New range is included in this range, do nothing.
                    } else if range.start() > new.start() && range.end() < new.end() {
                        new_ranges.push(*new.start()..=*range.start() - 1);
                        new_ranges.push(*range.end() + 1..=*new.end());
                    } else if range.start() <= new.start() && range.end() >= new.start() {
                        new_ranges.push(*range.end() + 1..=*new.end());
                    } else if range.start() <= new.end() && range.end() >= new.end() {
                        new_ranges.push(*new.start()..=*range.start() - 1);
                    } else {
                        new_ranges.push(new);
                    }
                }
            }
            ranges.extend_from_slice(&new_ranges);
        }

        let sol1 = lines.filter(|line| {
            let id = line.parse::<u64>().unwrap();
            ranges.iter().any(|range| range.contains(&id))
        }).count() as _;
        let sol2 = ranges.into_iter().map(|range| *range.end() - *range.start() + 1).sum();

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::U32(14)],
            input: "3-5
10-14
16-20
12-18

1
5
8
11
17
32
",
        },
    ];
}
