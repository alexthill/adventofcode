use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use std::collections::HashMap;

fn rescale_map(w: &mut i32, h: &mut i32, map: &[u8]) -> Vec<u8> {
    let w_old = *w as usize;
    let h_old = *h as usize;
    *w += 2;
    *h += 2;
    let mut new = vec![b'.'; (*w * *h) as usize];
    for y in 0..h_old {
        for x in 0..w_old - 1 {
            new[x + 1 + (y + 1) * *w as usize] = map[x + y * w_old];
        }
    }
    for y in 1..=*h {
        new[(*w * y - 1) as usize] = b'\n';
    }
    new
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Clean,
    Weakened,
    Infected,
    Flagged,
}
impl State {
    fn next(self) -> Self {
        match self {
            Self::Clean => Self::Weakened,
            Self::Weakened => Self::Infected,
            Self::Infected => Self::Flagged,
            Self::Flagged => Self::Clean,
        }
    }
}

pub struct Day22;

impl Day for Day22 {

    const PART1: Solution = Solution::U32(5352);
    const PART2: Solution = Solution::U32(2511475);

    fn solve(input: &str) -> [Solution; 2] {
        // Part 1 is implemented using an array that is rescaled on demand.
        // Part 2 uses a HashMap.
        // Don't know which will be faster, so leaving both for the moment.

        let sol1 = {
            let mut w = input.bytes().position(|c| c == b'\n').unwrap() as i32 + 1;
            let mut h = (input.len() as i32 + 1) / w;
            let mut map = input.bytes().collect::<Vec<_>>();
            map.push(b'\n');
            let mut pos = IVec2::from([0, 0]);
            let mut dir = IVec2::from([0, -1]);
            let mut infected = 0;
            let mut i = 0;
            while i < 10000 {
                let idx = pos.x() + w / 2 - 1 + (pos.y() + h / 2) * w;
                if !(0..map.len() as i32).contains(&idx) {
                    map = rescale_map(&mut w, &mut h, &map);
                    continue;
                }
                let cell = &mut map[idx as usize];
                if *cell == b'\n' {
                    map = rescale_map(&mut w, &mut h, &map);
                    continue;
                }

                if *cell == b'#' {
                    dir = IVec2::from([-dir.y(), dir.x()]);
                    *cell = b'.';
                } else {
                    dir = IVec2::from([dir.y(), -dir.x()]);
                    *cell = b'#';
                    infected += 1;
                }
                pos += dir;
                i += 1;
            }
            infected
        };

        let sol2 = {
            let w = input.bytes().position(|c| c == b'\n').unwrap() as i32 + 1;
            let h = (input.len() as i32 + 1) / w;
            let mut map = HashMap::new();
            for (y, line) in input.lines().enumerate() {
                for (x, c) in line.bytes().enumerate() {
                    if c == b'#' {
                        let pos = IVec2::from([
                            x as i32 - w / 2 + 1,
                            y as i32 - h / 2,
                        ]);
                        map.insert(pos, State::Infected);
                    }
                }
            }
            let mut pos = IVec2::from([0, 0]);
            let mut dir = IVec2::from([0, -1]);
            let mut infected = 0;
            for _ in 0..10_000_000 {
                let cell = map.entry(pos).or_insert(State::Clean);
                dir = match *cell {
                    State::Clean => IVec2::from([dir.y(), -dir.x()]),
                    State::Weakened => IVec2::from([dir.x(), dir.y()]),
                    State::Infected => IVec2::from([-dir.y(), dir.x()]),
                    State::Flagged => IVec2::from([-dir.x(), -dir.y()]),
                };
                *cell = cell.next();
                infected += (*cell == State::Infected) as u32;
                pos += dir;
            }
            infected
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5587), Solution::U32(2511944)],
            input: "..#
#..
...
",
        },
    ];
}
