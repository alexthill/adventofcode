use aoc_lib_rust::{Day, Example, Solution};
use std::collections::hash_map::{Entry, HashMap};

pub struct Day06;

impl Day for Day06 {

    const PART1: Solution = Solution::U32(12841);
    const PART2: Solution = Solution::U32(8038);

    fn solve(input: &str) -> [Solution; 2] {
        let mut banks = input.split_ascii_whitespace()
            .map(|x| x.parse::<u8>().unwrap())
            .collect::<Box<[u8]>>();

        let mut seen = HashMap::new();
        let mut round = 0;
        let (sol1, sol2) = loop {
            match seen.entry(banks.clone()) {
                Entry::Occupied(entry) => {
                    let prev = *entry.get();
                    break (round, round - prev);
                }
                Entry::Vacant(entry) => entry.insert(round),
            };
            round += 1;
            let (mut idx, max) = banks.iter().copied()
                .enumerate()
                .max_by(|a, b| a.1.cmp(&b.1).then(a.0.cmp(&b.0).reverse()))
                .unwrap();
            banks[idx] = 0;
            for _ in 0..max {
                idx = (idx + 1) % banks.len();
                banks[idx] += 1;
            }
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::U32(4)],
            input: "0 2 7 0",
        },
    ];
}
