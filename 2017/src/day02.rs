use aoc_lib_rust::{Day, Example, Solution};

pub struct Day02;

impl Day for Day02 {

    const PART1: Solution = Solution::U32(45351);
    const PART2: Solution = Solution::U32(275);

    fn solve(input: &str) -> [Solution; 2] {
        let (sol1, sol2) = input.lines().fold((0, 0), |acc, line| {
            let nums = line.split([' ', '\t']).map(|part| {
                part.parse::<u32>().unwrap()
            }).collect::<Vec<_>>();
            let min = nums.iter().min().unwrap();
            let max = nums.iter().max().unwrap();

            let mut div = 0;
            'outer: for (i, a) in nums.iter().enumerate() {
                for b in nums[i + 1..].iter() {
                    if a % b == 0 {
                        div = a / b;
                        break 'outer;
                    } else if b % a == 0 {
                        div = b / a;
                        break 'outer;
                    }
                }
            }

            (acc.0 + max - min, acc.1 + div)
        });

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(18), Solution::U32(9)],
            input: "5 9 2 8
9 4 7 3
3 8 6 5",
        },
    ];
}
