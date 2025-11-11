use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;

pub struct Day19;

impl Day for Day19 {

    const PART1: Solution = Solution::U32(1834471);
    const PART2: Solution = Solution::U32(1420064);

    fn solve(input: &str) -> [Solution; 2] {
        let n = input.parse::<u32>().unwrap();

        let mut queue = (1..=n).collect::<VecDeque<_>>();
        let sol1 = loop {
            let a = queue.pop_front().unwrap();
            if queue.pop_front().is_some() {
                queue.push_back(a);
            } else {
                break a;
            }
        };

        let mut queue = (1..=n).collect::<Vec<_>>();
        let mut start = 0;
        while queue.len() > 1 {
            let next_start = (start + queue.len() / 2) % queue.len();
            let mut removed_before_next_start = 0;
            for i in 0..queue.len() / 2 {
                let rm_idx = (i + start + (i + queue.len()) / 2) % queue.len();
                queue[rm_idx] = 0;
                removed_before_next_start += (rm_idx < next_start) as usize;
            }
            start = next_start - removed_before_next_start;
            queue.retain(|el| *el != 0);
        };
        let sol2 = queue[0];

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::U32(2)],
            input: "5",
        },
        Example {
            solution: [Solution::U32(5), Solution::U32(3)],
            input: "6",
        },
        Example {
            solution: [Solution::U32(7), Solution::U32(5)],
            input: "7",
        },
    ];
}
