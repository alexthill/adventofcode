use aoc_lib_rust::{Day, Example, Solution};

pub struct Day03;

impl Day for Day03 {

    const PART1: Solution = Solution::U32(17316);
    const PART2: Solution = Solution::U64(171741365473332);

    fn solve(input: &str) -> [Solution; 2] {
        let sol1 = input.as_bytes().split(|ch| *ch == b'\n').map(|line| {
            let (a_idx, a) = line[..line.len() - 1].iter()
                .enumerate()
                .max_by(|a, b| a.1.cmp(b.1).then(a.0.cmp(&b.0).reverse()))
                .unwrap();
            let b = line[a_idx + 1..].iter().max().unwrap();
            (a - b'0') as u32 * 10 + (b - b'0') as u32
        }).sum();

        let sol2 = input.as_bytes().split(|ch| *ch == b'\n').map(|line| {
            let mut joltage = 0;
            let mut idx = 0;
            for i in (1..12).rev() {
                let (new_idx, val) = line[..line.len() - i].iter()
                    .enumerate()
                    .skip(idx)
                    .max_by(|a, b| a.1.cmp(b.1).then(a.0.cmp(&b.0).reverse()))
                    .unwrap();
                idx = new_idx + 1;
                joltage = joltage * 10 + (val - b'0') as u64;
            }
            let b = line[idx..].iter().max().unwrap();
            joltage * 10 + (b - b'0') as u64
        }).sum();

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(357), Solution::U64(3121910778619)],
            input: "987654321111111
811111111111119
234234234234278
818181911112111",
        },
    ];
}
