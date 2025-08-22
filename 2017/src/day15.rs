use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next, next_parse};

pub struct Day15;

impl Day for Day15 {

    const PART1: Solution = Solution::U32(567);
    const PART2: Solution = Solution::U32(323);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.lines();
        let start_a = next_parse!(next!(lines).split(' ').skip(4), u64);
        let start_b = next_parse!(next!(lines).split(' ').skip(4), u64);

        let mut val_a = start_a;
        let mut val_b = start_b;
        let mut sol1 = 0;
        for _ in 0..40_000_000 {
            val_a = (val_a * 16807) % 2147483647;
            val_b = (val_b * 48271) % 2147483647;
            sol1 += (val_a & u16::MAX as u64 == val_b & u16::MAX as u64) as u32;
        }

        let mut val_a = start_a;
        let mut val_b = start_b;
        let mut sol2 = 0;
        for _ in 0..5_000_000 {
            val_a = (val_a * 16807) % 2147483647;
            val_b = (val_b * 48271) % 2147483647;
            while val_a % 4 != 0 {
                val_a = (val_a * 16807) % 2147483647;
            }
            while val_b % 8 != 0 {
                val_b = (val_b * 48271) % 2147483647;
            }
            sol2 += (val_a & u16::MAX as u64 == val_b & u16::MAX as u64) as u32;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(588), Solution::U32(309)],
            input: "Generator A starts with 65
Generator B starts with 8921",
        },
    ];
}
