use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day03;

impl Day for Day03 {

    const PART1: Solution = Solution::U32(403);
    const PART2: Solution = Solution::U32(4158);

    fn solve(input: &str) -> [Solution; 2] {
        let mut line_iter = input.as_bytes().split(|c| *c == b'\n');
        let lines = [line_iter.next().unwrap(), line_iter.next().unwrap()];
        debug_assert!(line_iter.next().is_none());

        fn parse_part(part: &[u8]) -> ((i32, i32), u32) {
            let dir = match part[0] {
                b'R' => (1, 0),
                b'L' => (-1, 0),
                b'U' => (0, 1),
                b'D' => (0, -1),
                _ => unreachable!(),
            };
            let len = part[1..].iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as u32);
            (dir, len)
        }

        let wire = {
            let mut wire = HashMap::new();
            let mut pos = (0_i32, 0_i32);
            let mut time = 0_u32;
            for part in lines[0].split(|c| *c == b',') {
                let (dir, len) = parse_part(part);
                for _ in 0..len {
                    pos.0 += dir.0;
                    pos.1 += dir.1;
                    time += 1;
                    wire.entry(pos).or_insert(time);
                }
            }
            wire
        };

        let mut pos = (0_i32, 0_i32);
        let mut time = 0_u32;
        let mut sol1 = u32::MAX;
        let mut sol2 = u32::MAX;
        for part in lines[1].split(|c| *c == b',') {
            let (dir, len) = parse_part(part);
            for _ in 0..len {
                pos.0 += dir.0;
                pos.1 += dir.1;
                time += 1;
                if let Some(time1) = wire.get(&pos) {
                    sol1 = sol1.min(pos.0.abs() as u32 + pos.1.abs() as u32);
                    sol2 = sol2.min(*time1 + time);
                }
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(6), Solution::U32(30)],
            input: "R8,U5,L5,D3
U7,R6,D4,L4",
        },
        Example {
            solution: [Solution::U32(159), Solution::U32(610)],
            input: "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83",
        },
        Example {
            solution: [Solution::U32(135), Solution::U32(410)],
            input: "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        },
    ];
}
