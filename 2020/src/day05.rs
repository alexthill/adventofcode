use aoc_lib_rust::{Day, Example, Solution};

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::U32(947);
    const PART2: Solution = Solution::U32(636);

    fn solve(input: &str) -> [Solution; 2] {
        let mut max = 0;
        let mut min = 1000;
        let mut seats = [true; 1000];
        for line in input.as_bytes().split(|c| *c == b'\n') {
            let mut id = 0;
            for (i, c) in line[..7].iter().enumerate() {
                if *c == b'B' {
                    id |= 8 << (6 - i);
                }
            }
            for (i, c) in line[7..].iter().enumerate() {
                if *c == b'R' {
                    id |= 1 << (2 - i);
                }
            }
            max = max.max(id);
            min = min.min(id);
            seats[id] = false;
        }

        let seat = seats[min..].iter().position(|s| *s).unwrap() + min;

        [Solution::U32(max as u32), Solution::U32(seat as u32)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(567), Solution::None],
            input: "BFFFBBFRRR",
        },
        Example {
            solution: [Solution::U32(119), Solution::None],
            input: "FFFBBBFRRR",
        },
        Example {
            solution: [Solution::U32(820), Solution::None],
            input: "BBFFBBFRLL",
        },
        Example {
            solution: [Solution::U32(820), Solution::None],
            input: "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL",
        },
    ];
}
