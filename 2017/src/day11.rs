use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;

pub struct Day11;

impl Day for Day11 {

    const PART1: Solution = Solution::U32(670);
    const PART2: Solution = Solution::U32(1426);

    fn solve(input: &str) -> [Solution; 2] {
        let mut pos = IVec2::default();
        let mut sol2 = 0;
        for dir in input.split(',') {
            pos += (match dir {
                "n"  => [ 0,  2],
                "ne" => [ 1,  1],
                "se" => [ 1, -1],
                "s"  => [ 0, -2],
                "sw" => [-1, -1],
                "nw" => [-1,  1],
                _ => unreachable!(),
            }).into();
            sol2 = sol2.max(pos.x().abs() + (pos.y().abs() - pos.x().abs()).max(0) / 2);
        };

        let sol1 = pos.x().abs() + (pos.y().abs() - pos.x().abs()).max(0) / 2;

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "ne,ne,ne",
        },
        Example {
            solution: [Solution::U32(0), Solution::None],
            input: "ne,ne,sw,sw",
        },
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: "ne,ne,s,s",
        },
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "se,sw,se,sw,sw",
        },
    ];
}
