use aoc_lib_rust::{Day, Example, Solution};

pub struct Day01;

impl Day for Day01 {

    const PART1: Solution = Solution::U32(1055);
    const PART2: Solution = Solution::U32(6386);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = 0;
        let mut sol2 = 0;
        let mut pos = 50;

        for line in input.lines() {
            let mut val = line[1..].parse::<i32>().unwrap();
            sol2 += (val / 100) as u32;
            val %= 100;
            match line.as_bytes()[0] {
                b'L' => {
                    sol2 += (pos <= val && pos != 0) as u32;
                    pos = (pos - val).rem_euclid(100);
                }
                b'R' => {
                    sol2 += (pos + val >= 100) as u32;
                    pos = (pos + val).rem_euclid(100);
                }
                _ => unreachable!(),
            }
            sol1 += (pos == 0) as u32;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::U32(6)],
            input: "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
",
        },
    ];
}
