use aoc_lib_rust::{Day, Example, Solution};

pub struct Day06;

impl Day for Day06 {

    const PART1: Solution = Solution::U32(6583);
    const PART2: Solution = Solution::U32(3290);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = 0;
        let mut sol2 = 0;
        let mut answers_any = 0_u32;
        let mut answers_all = !0_u32;

        for line in input.as_bytes().split(|c| *c == b'\n') {
            if line.is_empty() {
                sol1 += answers_any.count_ones();
                sol2 += answers_all.count_ones();
                answers_any = 0;
                answers_all = !0;
                continue;
            }
            let answers = line.iter().fold(0, |acc, c| acc | (1 << (*c - b'a')));
            answers_all &= answers;
            answers_any |= answers;
        }
        sol1 += answers_any.count_ones();
        sol2 += answers_all.count_ones();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(11), Solution::U32(6)],
            input: "abc

a
b
c

ab
ac

a
a
a
a

b",
        },
    ];
}


