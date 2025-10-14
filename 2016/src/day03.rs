use aoc_lib_rust::{next_parse, Day, Example, Solution};

pub struct Day03;

impl Day for Day03 {

    const PART1: Solution = Solution::U32(993);
    const PART2: Solution = Solution::U32(1849);

    fn solve(input: &str) -> [Solution; 2] {
        let sol1 = input.lines().filter(|line| {
            let mut parts = line.split(' ').filter(|part| !part.is_empty());
            let a = next_parse!(parts, u32);
            let b = next_parse!(parts, u32);
            let c = next_parse!(parts, u32);
            a + b > c && a + c > b && b + c > a
        }).count();

        let sol2 = input.lines().collect::<Vec<&str>>().chunks(3).map(|lines| {
            let nums = [0, 1, 2].map(|n| {
                let mut parts = lines[n].split(' ').filter(|part| !part.is_empty());
                [next_parse!(parts, u32), next_parse!(parts, u32), next_parse!(parts, u32)]

            });
            (0..3).filter(|&n| {
                let a = nums[0][n];
                let b = nums[1][n];
                let c = nums[2][n];
                a + b > c && a + c > b && b + c > a
            }).count() as u32
        }).sum();

        [Solution::U32(sol1 as _), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1), Solution::None],
            input: " 5 10 25
10  3  3
 3  4  5",
        },
    ];
}
