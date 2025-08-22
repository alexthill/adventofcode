use aoc_lib_rust::{Day, Example, Solution};

pub struct Day17;

impl Day for Day17 {

    const PART1: Solution = Solution::U32(180);
    const PART2: Solution = Solution::U32(13326437);

    fn solve(input: &str) -> [Solution; 2] {
        let step = input.parse::<usize>().unwrap() + 1;

        let mut buf = vec![0];
        let mut curr = 0_usize;
        for i in 1..2018 {
            curr = (curr + step) % buf.len();
            if curr == 0 {
                curr = buf.len();
                buf.push(i);
            } else {
                buf.insert(curr, i);
            }
        }
        let idx = buf.iter().position(|x| *x == 2017).unwrap();
        let sol1 = buf[(idx + 1) % buf.len()];

        let mut len = buf.len();
        let mut sol2 = 1;
        while len < 50_000_001 {
            curr = curr + step;
            if curr > len {
                curr %= len;
            }
            if curr == 1 {
                sol2 = len as u32;
            }
            len += 1;

            let skips = (len - curr) / step;
            curr = curr + step * skips;
            len += skips;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(638), Solution::None],
            input: "3",
        },
    ];
}
