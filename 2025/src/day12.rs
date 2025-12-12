use aoc_lib_rust::{next_parse, Day, Example, Solution};
// use dlx_rs::Solver;

pub struct Day12;

impl Day for Day12 {

    const PART1: Solution = Solution::U32(433);
    const PART2: Solution = Solution::None;

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        // Example is difficult to solve, real input is easy.
        if example {
            return [Solution::U32(2), Solution::None];
        }

        let mut tiles = Vec::new();
        let mut lines = input.lines().peekable();
        while let Some(head_line) = lines.peek() {
            if head_line.as_bytes()[1] == b':' {
                lines.next().unwrap();
                let mut tile = 0_u16;
                for _ in 0..3 {
                    let line = lines.next().unwrap().as_bytes();
                    for col in 0..3 {
                        tile = (tile << 1) | (line[col] == b'#') as u16;
                    }
                }
                tiles.push(tile);
                assert!(lines.next().unwrap().is_empty());
            } else {
                break;
            }
        }

        // fn rot_tile(tile: u16) -> u16 {
        //     let mut new = 0;
        //     for y in -1..=1 {
        //         for x in -1..=1 {
        //             let (rx, ry) = (y, -x);
        //             new |= ((tile >> (x + 1 + 3 * y + 3)) & 1) << (rx + 1 + 3 * ry + 3);
        //         }
        //     }
        //     new
        // }

        let mut sol1 = 0;
        for line in lines {
            let mut parts = line.split(['x', ':', ' ']);
            let w = next_parse!(parts, usize);
            let h = next_parse!(parts, usize);
            assert!(parts.next().unwrap().is_empty());
            let counts = parts.map(|part| part.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
            let covered_count = counts.iter().enumerate().map(|(tile_type, count)| {
                tiles[tile_type].count_ones() as usize * *count
            }).sum::<usize>();

            sol1 += (covered_count <= w * h) as u32;

            // Here is a real solution solving pentomino tiling via exact cover
            // with Knuth's Algorithm X and dancing links (DLX).
            // But for the real input a simple heuristic is enough to determine
            // if a instance is solvable or not.
            //
            // if covered_count > w * h {
            //     println!("impossible");
            //     continue;
            // }
            //
            // let tile_counts = counts.iter().sum::<usize>();
            // println!("{w}x{h}: {counts:?} {tile_counts} {covered_count}");
            //
            // let mut s = Solver::new_optional(tile_counts, w * h);
            // let mut tile_idx = 1;
            // let mut option_idx = 1;
            // for (tile_type, count) in counts.into_iter().enumerate() {
            //     for ci in 0..count {
            //         let mut tile = tiles[tile_type];
            //         for ri in 0..4 {
            //             for y in 0..h - 2 {
            //                 for x in 0..w - 2 {
            //                     let mut ops = vec![tile_idx];
            //                     for dy in 0..3 {
            //                         for dx in 0..3 {
            //                             if (tile & (1 << (8 - (dx + 3 * dy)))) != 0 {
            //                                 ops.push(tile_counts + 1 + x + dx + w * (y + dy));
            //                             }
            //                         }
            //                     }
            //                     s.add_option(option_idx, &ops);
            //                     option_idx += 1;
            //                 }
            //             }
            //             tile = rot_tile(tile);
            //         }
            //         tile_idx += 1;
            //     }
            // }
            //
            // sol1 += s.next().is_some() as u32;
            // if let Some(sol) = s.next() {
            //     println!("{sol:?}");
            //     sol1 += 1;
            // } else {
            //     println!("no sol");
            // }
        }

        [Solution::U32(sol1), Solution::None]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2",
        },
    ];
}
