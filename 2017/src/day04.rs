use aoc_lib_rust::{Day, Example, Solution};

pub struct Day04;

impl Day for Day04 {

    const PART1: Solution = Solution::U32(337);
    const PART2: Solution = Solution::U32(231);

    fn solve(input: &str) -> [Solution; 2] {
        let (sol1, sol2) = input.lines().fold((0, 0), |mut acc, line| {
            let mut words = line.split(' ').collect::<Vec<_>>();
            let count = words.len();
            words.sort_unstable();
            words.dedup();
            if words.len() == count {
                acc.0 += 1;
                let mut words = words.into_iter().map(|word| {
                    let mut word = word.bytes().collect::<Vec<_>>();
                    word.sort_unstable();
                    word
                }).collect::<Vec<_>>();
                let count = words.len();
                words.sort_unstable();
                words.dedup();
                acc.1 += (words.len() == count) as u32;
            }
            acc
        });

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::U32(3)],
            input: "aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa
abc aaa cba
ab bc
abc bac",
        },
    ];
}
