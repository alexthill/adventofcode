use aoc_lib_rust::{Day, Example, Solution};

pub struct Day12;

impl Day for Day12 {

    const PART1: Solution = Solution::U32(1631);
    const PART2: Solution = Solution::U32(58606);

    fn solve(input: &str) -> [Solution; 2] {
        let mut pos1 = (0, 0); // (+E/-W, +N/-S)
        let mut dir = (1, 0);

        let mut pos2 = (0, 0);
        let mut way = (10, 1);

        for line in input.as_bytes().split(|c| *c == b'\n') {
            let n = line[1..].iter().fold(0_i32, |acc, c| acc * 10 + (*c - b'0') as i32);
            match line[0] {
                b'N' => { pos1.1 += n; way.1 += n }
                b'S' => { pos1.1 -= n; way.1 -= n }
                b'E' => { pos1.0 += n; way.0 += n }
                b'W' => { pos1.0 -= n; way.0 -= n }
                b'L' => {
                    for _ in 0..n / 90 {
                        dir = (-dir.1, dir.0);
                        way = (-way.1, way.0);
                    }
                }
                b'R' => {
                    for _ in 0..n / 90 {
                        dir = (dir.1, -dir.0);
                        way = (way.1, -way.0);
                    }
                }
                b'F' => {
                    pos1.0 += n * dir.0;
                    pos1.1 += n * dir.1;
                    pos2.0 += n * way.0;
                    pos2.1 += n * way.1;
                },
                _ => unreachable!(),
            }
        }

        let sol1 = pos1.0.abs() + pos1.1.abs();
        let sol2 = pos2.0.abs() + pos2.1.abs();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(25), Solution::U32(286)],
            input: "F10
N3
F7
R90
F11",
        },
    ];
}
