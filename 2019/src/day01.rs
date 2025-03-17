use aoc_lib_rust::{Day, Example, Solution};

pub struct Day01;

impl Day for Day01 {

    const PART1: Solution = Solution::U32(3474920);
    const PART2: Solution = Solution::U32(5209504);

    fn solve(input: &str) -> [Solution; 2] {
        let (sol1, sol2) = input.lines()
            .map(|line| {
                let sol1 = line.parse::<u32>().unwrap() / 3 - 2;
                let mut sol2 = sol1;
                let mut fuel = sol2 / 3;
                loop {
                    if fuel <= 2 {
                        break;
                    }
                    fuel -= 2;
                    sol2 += fuel;
                    fuel /= 3;
                }
                (sol1, sol2)
            })
            .fold((0, 0), |(acc_a, acc_b), (a, b)| (acc_a + a, acc_b + b));

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::U32(2)],
            input: "12",
        },
        Example {
            solution: [Solution::U32(2), Solution::U32(2)],
            input: "14",
        },
        Example {
            solution: [Solution::U32(654), Solution::U32(966)],
            input: "1969",
        },
        Example {
            solution: [Solution::U32(33583), Solution::U32(50346)],
            input: "100756",
        },
        Example {
            solution: [Solution::U32(34241), Solution::U32(51316)],
            input: "12
14
1969
100756",
        },
    ];
}
