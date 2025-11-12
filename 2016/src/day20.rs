use aoc_lib_rust::{next_parse, Day, Example, Solution};

pub struct Day20;

impl Day for Day20 {

    const PART1: Solution = Solution::U32(23923783);
    const PART2: Solution = Solution::U32(125);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let max_valid = if example { 9 } else { u32::MAX };
        let mut ranges = input.lines().map(|line| {
            let mut parts = line.split('-');
            next_parse!(parts, u32)..=next_parse!(parts, u32)
        }).collect::<Vec<_>>();
        ranges.sort_unstable_by_key(|range| *range.start());

        let mut high = 0;
        let mut sol1 = u32::MAX;
        let mut sol2 = 0;
        for range in ranges.iter() {
            if *range.start() > high + 1 {
                sol1 = sol1.min(high + 1);
                sol2 += *range.start() - high - 1;
            }
            high = high.max(*range.end());
            if high == max_valid {
                break;
            }
        }
        sol2 += max_valid - high;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::U32(2)],
            input: "5-8
0-2
4-7",
        },
    ];
}
