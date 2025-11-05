use aoc_lib_rust::{next_parse, Day, Example, Solution};

pub struct Day15;

impl Day for Day15 {

    const PART1: Solution = Solution::U32(121834);
    const PART2: Solution = Solution::U32(3208099);

    #[allow(clippy::manual_is_multiple_of)]
    fn solve(input: &str) -> [Solution; 2] {
        let discs = input.lines().map(|line| {
            let mut parts = line["Disc #1 has ".len()..line.len() - 1]
                .split(" positions; at time=0, it is at position ");
            (next_parse!(parts, u32), next_parse!(parts, u32))
        }).collect::<Vec<_>>();

        let mut i = 0;
        let sol1 = 'outer: loop {
            for (j, (count, pos)) in discs.iter().copied().enumerate() {
                if (pos + i + j as u32 + 1) % count != 0 {
                    i += 1;
                    continue 'outer;
                }
            }
            break i;
        };

        let period: u32 = discs.iter().map(|disc| disc.0).product();
        let mut i = 0;
        let sol2 = 'outer: loop {
            let pos = sol1 + period * i;
            if (pos + discs.len() as u32 + 1) % 11 != 0 {
                i += 1;
                continue 'outer;
            }
            break pos;
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::U32(85)],
            input: "Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.
",
        },
    ];
}
