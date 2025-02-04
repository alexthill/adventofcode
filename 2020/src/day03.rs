use aoc_lib_rust::{Day, Example, Solution};

pub struct Day03;

impl Day for Day03 {

    const PART1: Solution = Solution::U32(259);
    const PART2: Solution = Solution::U32(2224913600);

    fn solve(input: &str) -> [Solution; 2] {
        let input = input.as_bytes();
        let w = input.iter().position(|c| *c == b'\n').unwrap();
        let h = (input.len() + 1) / (w + 1);

        let trees_hit = [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)].map(|slope| {
            let mut pos = (0, 0);
            let mut trees = 0;
            while pos.1 < h {
                trees += (input[(pos.0 % w) + pos.1 * (w + 1)] == b'#') as u32;
                pos.0 += slope.0;
                pos.1 += slope.1;
            }
            trees
        });

        let sol1 = trees_hit[0];
        let sol2 = trees_hit.into_iter().product::<u32>();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(7), Solution::U32(336)],
            input: "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#",
        },
    ];
}
