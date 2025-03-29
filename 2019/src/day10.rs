use aoc_lib_rust::{Day, Example, Solution};

pub struct Day10;

impl Day for Day10 {

    const PART1: Solution = Solution::U32(344);
    const PART2: Solution = Solution::U32(2732);

    fn solve(input: &str) -> [Solution; 2] {
        let map = input.as_bytes().split(|c| *c == b'\n')
            .map(|line| line.to_vec())
            .collect::<Vec<_>>();
        let h = map.len() as isize;
        let w = map[0].len() as isize;

        let check = |map: &[Vec<u8>], mut pos: [isize; 2], dir: [isize; 2]| {
            loop {
                pos[0] += dir[0];
                pos[1] += dir[1];
                if !(0..w).contains(&pos[0]) || !(0..h).contains(&pos[1]) {
                    break None;
                }
                if map[pos[1] as usize][pos[0] as usize] == b'#' {
                    break Some(pos);
                }
            }
        };

        let mut dirs = Vec::new();
        for dy in -h + 1..h {
            'outer: for dx in -w + 1..w {
                if dy == 0 && dx == 0 {
                    continue;
                }
                for i in std::iter::once(2).chain(2..w.min(h)).step_by(2) {
                    if dx % i == 0 && dy % i == 0 {
                        continue 'outer;
                    }
                }
                dirs.push([dx, dy]);
            }
        }
        dirs.sort_unstable_by_key(|[dx, dy]| {
            let dy = *dy as f32;
            let len = ((*dx as f32).powi(2) + dy * dy).sqrt();
            let dot = -dy / len;
            let key = if *dx >= 0 { -dot } else { dot + 2.0 };
            (key * 1_000_000_000.0) as i64
        });

        let mut sol1 = 0;
        let mut coords = [0; 2];
        for (y, row) in map.iter().enumerate() {
            for (x, c) in row.iter().enumerate() {
                if *c != b'#' {
                    continue;
                }

                let pos = [x as isize, y as isize];
                let mut count = 0;
                for &dir in dirs.iter() {
                    count += check(&map, pos, dir).is_some() as u32;
                }

                if count > sol1 {
                    sol1 = count;
                    coords = pos;
                }
            }
        }

        let mut map = map;
        let mut i = 0;
        let sol2 = 'outer: loop {
            let old_i = i;
            for &dir in dirs.iter() {
                if let Some([x, y]) = check(&map, coords, dir) {
                    i += 1;
                    map[y as usize][x as usize] = b'.';
                    if i == 200 {
                        break 'outer x * 100 + y;
                    }
                }
            }
            if i == old_i {
                break 0;
            }
        };

        [Solution::U32(sol1), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(8), Solution::None],
            input: ".#..#
.....
#####
....#
...##",
        },
        Example {
            solution: [Solution::U32(33), Solution::None],
            input: "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####",
        },
        Example {
            solution: [Solution::U32(210), Solution::U32(802)],
            input: ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##",
        },
    ];
}
