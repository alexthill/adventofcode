use aoc_lib_rust::{Day, Example, Solution};

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::U32(372671);
    const PART2: Solution = Solution::U32(25608480);

    fn solve(input: &str) -> [Solution; 2] {
        let mut jumps = input.lines()
            .map(|line| line.parse::<isize>().unwrap())
            .collect::<Vec<_>>();
        let mut jumps2 = jumps.clone();

        let mut ip = 0;
        let mut i = 0;
        let sol1 = loop {
            let Some(jump) = ip.try_into().ok().and_then(|ip: usize| jumps.get_mut(ip)) else {
                break i;
            };
            ip += *jump;
            *jump += 1;
            i += 1;
        };

        let mut ip = 0;
        let mut i = 0;
        let sol2 = loop {
            let Some(jump) = ip.try_into().ok().and_then(|ip: usize| jumps2.get_mut(ip)) else {
                break i;
            };
            ip += *jump;
            *jump += (*jump < 3) as isize * 2 - 1;
            i += 1;
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::U32(10)],
            input: "0
3
0
1
-3",
        },
    ];
}
