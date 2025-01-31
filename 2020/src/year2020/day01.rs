use aoc_lib_rust::{Day, Example, Solution};

pub struct Day01;

impl Day for Day01 {

    const PART1: Solution = Solution::U32(787776);
    const PART2: Solution = Solution::U32(262738554);

    fn solve(input: &str) -> [Solution; 2] {
        let nums = input.split('\n')
            .filter_map(|s| s.parse::<u32>().ok())
            .collect::<Vec<_>>();

        let mut sol1 = Solution::None;
        'outer: for (i, &num1) in nums.iter().enumerate() {
            if num1 > 2020 {
                continue;
            }
            for &num2 in nums[i..].iter() {
                if num1 + num2 == 2020 {
                    sol1 = Solution::U32(num1 * num2);
                    break 'outer;
                }
            }
        }

        let mut sol2 = Solution::None;
        'outer: for (i, &num1) in nums.iter().enumerate() {
            if num1 > 2020 {
                continue;
            }
            for (j, &num2) in nums[i..].iter().enumerate() {
                if num1 + num2 > 2020 {
                    continue;
                }
                for &num3 in nums[j..].iter() {
                    if num1 + num2 + num3 == 2020 {
                        sol2 = Solution::U32(num1 * num2 * num3);
                        break 'outer;
                    }
                }
            }
        }

        [sol1, sol2]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(514579), Solution::U32(241861950)],
            input: "1721
979
366
299
675
1456",
        },
    ];
}
