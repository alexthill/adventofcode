use aoc_lib_rust::{Day, Example, Solution};

pub struct Day02;

impl Day for Day02 {

    const PART1: Solution = Solution::U32(18843);
    const PART2: Solution = Solution::Str("67BB9");

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = 0;
        let mut pos: u32 = 5;
        for line in input.lines() {
            for ch in line.bytes() {
                pos = match ch {
                    b'U' if pos > 3 => pos - 3,
                    b'L' if pos != 1 && pos != 4 && pos != 7 => pos - 1,
                    b'R' if pos != 3 && pos != 6 && pos != 9 => pos + 1,
                    b'D' if pos < 7 => pos + 3,
                    b'U' | b'L' | b'R' | b'D' => pos,
                    _ => unreachable!(),
                };
            }
            sol1 = sol1 * 10 + pos;
        }

        let mut sol2 = String::new();
        let mut pos: u32 = 5;
        for line in input.lines() {
            for ch in line.bytes() {
                pos = match ch {
                    b'U' if [3, 0xD].contains(&pos) => pos - 2,
                    b'U' if ![5, 2, 1, 4, 9].contains(&pos) => pos - 4,
                    b'L' if ![1, 2, 5, 0xA, 0xD].contains(&pos) => pos - 1,
                    b'R' if ![1, 4, 9, 0xC, 0xD].contains(&pos) => pos + 1,
                    b'D' if [1, 0xB].contains(&pos) => pos + 2,
                    b'D' if ![5, 0xA, 0xD, 0xC, 9].contains(&pos) => pos + 4,
                    b'U' | b'L' | b'R' | b'D' => pos,
                    _ => unreachable!(),
                };
            }
            sol2.push(char::from_digit(pos, 16).unwrap().to_ascii_uppercase());
        }

        [Solution::U32(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1985), Solution::Str("5DB3")],
            input: "ULL
RRDDD
LURDL
UUUUD",
        },
    ];
}
