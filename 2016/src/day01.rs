use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use std::collections::HashSet;

pub struct Day01;

impl Day for Day01 {

    const PART1: Solution = Solution::U32(273);
    const PART2: Solution = Solution::U32(115);

    fn solve(input: &str) -> [Solution; 2] {
        let mut pos = IVec2::new([0, 0]);
        let mut dir = IVec2::new([0, 1]);
        let mut visited = HashSet::new();
        let mut sol2 = None;

        for part in input.split(", ") {
            let dist = part[1..].parse::<i32>().unwrap();
            dir = match part.as_bytes()[0] {
                b'L' => [-dir.y(), dir.x()].into(),
                b'R' => [dir.y(), -dir.x()].into(),
                _ => unreachable!(),
            };
            if sol2.is_none() {
                for _ in 0..dist {
                    pos += dir;
                    if visited.contains(&pos) {
                        sol2 = Some(pos.x().abs() + pos.y().abs());
                    }
                    visited.insert(pos);
                }
            } else {
                pos += dir * dist;
            }
        }

        let sol1 = pos.x().abs() + pos.y().abs();

        [Solution::U32(sol1 as _), Solution::U32(sol2.unwrap_or(0) as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(12), Solution::None],
            input: "R5, L5, R5, R3",
        },
        Example {
            solution: [Solution::U32(8), Solution::U32(4)],
            input: "R8, R4, R4, R8",
        },
    ];
}
