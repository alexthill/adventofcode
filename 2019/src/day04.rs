use aoc_lib_rust::{Day, Example, Solution};

pub struct Day04;

impl Day for Day04 {

    const PART1: Solution = Solution::U32(2150);
    const PART2: Solution = Solution::U32(1462);

    fn solve(input: &str) -> [Solution; 2] {
        let mut parts = input.split('-');
        let a = parts.next().unwrap().parse::<u32>().unwrap();
        let b = parts.next().unwrap().parse::<u32>().unwrap();
        debug_assert!(parts.next().is_none());

        fn check1(min: u32, max: u32, n: u32, prev: u32, double: bool) -> u32 {
            if n > max {
                return 0;
            }
            let n10 = n * 10;
            if n >= min && n10 >= max {
                return double as _;
            }
            (prev..=9).map(|d| check1(min, max, n10 + d, d, double || d == prev)).sum()
        }
        let sol1 = (1..=9).map(|d| check1(a, b, d, d, false)).sum::<u32>();

        fn check2(min: u32, max: u32, n: u32, prev: u32, run: u8, double: bool) -> u32 {
            if n > max {
                return 0;
            }
            let n10 = n * 10;
            if n >= min && n10 >= max {
                return (double || run == 1) as _;
            }
            (prev..=9).map(|d| {
                let (run, double) = if d == prev {
                    (run + 1, double)
                } else {
                    (0, double || run == 1)
                };
                check2(min, max, n10 + d, d, run, double)
            }).sum()
        }
        let sol2 = (1..=9).map(|d| check2(a, b, d, d, 0, false)).sum::<u32>();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1), Solution::U32(1)],
            input: "10-12",
        },
        Example {
            solution: [Solution::U32(9), Solution::U32(8)],
            input: "110-120",
        },
    ];
}
