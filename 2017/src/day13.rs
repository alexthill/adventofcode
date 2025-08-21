use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::next_parse;

#[derive(Debug)]
struct Scanner {
    depth: u8,
    range: u8,
    range2: u8,
}

pub struct Day13;

impl Day for Day13 {

    const PART1: Solution = Solution::U32(648);
    const PART2: Solution = Solution::U32(3933124);

    fn solve(input: &str) -> [Solution; 2] {
        let scanners = input.lines().map(|line| {
            let mut parts = line.split(": ");
            let depth = next_parse!(parts, u8);
            let range = next_parse!(parts, u8);
            Scanner { depth, range, range2: range * 2 - 2 }
        }).collect::<Vec<_>>();

        let sol1 = scanners.iter().map(|scanner| {
            if scanner.depth % scanner.range2 == 0 {
                scanner.depth as u32 * scanner.range as u32
            } else {
                0
            }
        }).sum();

        let mut sol2 = 0;
        'outer: for delay in 0..1000000000 {
            for scanner in scanners.iter() {
                if (scanner.depth as u32 + delay) % scanner.range2 as u32 == 0 {
                    continue 'outer;
                }
            }
            sol2 = delay;
            break;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(24), Solution::None],
            input: "0: 3
1: 2
4: 4
6: 4",
        },
    ];
}
