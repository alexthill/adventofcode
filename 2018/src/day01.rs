use aoc_lib_rust::{Day, Example, Solution};

pub struct Day01;

impl Day for Day01 {

    const PART1: Solution = Solution::U32(474);
    const PART2: Solution = Solution::U32(137041);

    fn solve(input: &str) -> [Solution; 2] {
        let changes = input.lines().map(|line| line.parse::<i32>().unwrap()).collect::<Vec<_>>();

        let sol1 = changes.iter().sum::<i32>();

        let mut freq: i32 = 0;
        let mut sol2 = 0;
        let mut found = [false; 1000001];
        for change in changes.into_iter().cycle() {
            if found[(freq + 500000) as usize] {
                sol2 = freq;
                break;
            }
            found[(freq + 500000) as usize] = true;
            freq += change;
        }

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(0), Solution::U32(0)],
            input: "+1
-1",
        },
        Example {
            solution: [Solution::U32(4), Solution::U32(10)],
            input: "+3
+3
+4
-2
-4",
        },
    ];
}
