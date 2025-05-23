use aoc_lib_rust::{Day, Example, Solution};
use std::mem;

pub struct Day15;

impl Day for Day15 {

    const PART1: Solution = Solution::U32(1428);
    const PART2: Solution = Solution::U32(3718541);

    fn solve(input: &str) -> [Solution; 2] {
        const ROUNDS: u32 = 30_000_000;
        let mut start_nums = input.split(',')
            .map(|s| s.parse().unwrap())
            .collect::<Vec<u32>>();
        let start_n = start_nums.pop().unwrap();
        let mut last_said = Vec::new();
        last_said.resize(ROUNDS as _, 0);

        let mut i = 1;
        for &n in &start_nums {
            last_said[n as usize] = i;
            i += 1;
        }

        let mut last_n = start_n;
        let mut sol1 = 0;
        for i in i..ROUNDS {
            if i == 2020 {
                sol1 = last_n;
            }
            last_n = match mem::replace(&mut last_said[last_n as usize], i) {
                0 => 0,
                turn => i - turn,
            };
        }
        let sol2 = last_n;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(436), Solution::U32(175594)],
            input: "0,3,6",
        },
        Example {
            solution: [Solution::U32(1), Solution::U32(2578)],
            input: "1,3,2",
        },
        Example {
            solution: [Solution::U32(10), Solution::U32(3544142)],
            input: "2,1,3",
        },
    ];
}
