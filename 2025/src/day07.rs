use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day07;

impl Day for Day07 {

    const PART1: Solution = Solution::U32(1703);
    const PART2: Solution = Solution::U64(171692855075500);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.as_bytes().split(|ch| *ch == b'\n');
        let start = lines.next().unwrap().iter()
            .position(|ch| *ch == b'S').unwrap();

        let mut beams = HashMap::from([(start, 1)]);
        let mut sol1 = 0;
        for line in lines {
            for (i, ch) in line.iter().enumerate() {
                if *ch == b'^'  {
                    let val = beams.remove(&i).unwrap_or(0);
                    if val != 0 {
                        sol1 += 1;
                        *beams.entry(i - 1).or_insert(0) += val;
                        *beams.entry(i + 1).or_insert(0) += val;
                    }
                }
            }
        }

        let sol2 = beams.values().sum();

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(21), Solution::U32(40)],
            input: ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............",
        },
    ];
}
