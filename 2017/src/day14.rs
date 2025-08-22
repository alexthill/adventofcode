use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use super::day10::knot_hash;

pub struct Day14;

impl Day for Day14 {

    const PART1: Solution = Solution::U32(8226);
    const PART2: Solution = Solution::U32(1128);

    fn solve(input: &str) -> [Solution; 2] {
        let mut grid = (0..128)
            .map(|i| knot_hash(format!("{input}-{i}").as_bytes()))
            .collect::<Vec<_>>();

        let sol1 = grid.iter().map(|row| row.count_ones()).sum();
        let mut sol2 = 0;
        let mut stack = Vec::new();
        for y in 0..128 {
            for x in 0..128 {
                if grid[y as usize] & (1 << x) == 0 {
                    continue;
                }
                sol2 += 1;
                stack.push(IVec2::from([x, y]));
                while let Some(pos) = stack.pop() {
                    grid[pos.y() as usize] &= !(1 << pos.x());
                    for dir in [[1, 0], [-1, 0], [0, 1], [0, -1]] {
                        let pos = pos + dir.into();
                        if !(0..128).contains(&pos.x()) || !(0..128).contains(&pos.y()) {
                            continue;
                        }
                        if grid[pos.y() as usize] & (1 << pos.x()) != 0 {
                            stack.push(pos);
                        }
                    }
                }
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(8108), Solution::U32(1242)],
            input: "flqrgnkx",
        },
    ];
}
