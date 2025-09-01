use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

fn rotate<const N: usize>(image: [[u8; N]; N]) -> [[u8; N]; N] {
    let mut out = [[0; N]; N];
    for y in 0..N {
        for x in 0..N {
            out[x][N - y - 1] = image[y][x];
        }
    }
    out
}

fn flip<const N: usize>(image: [[u8; N]; N]) -> [[u8; N]; N] {
    let mut out = [[0; N]; N];
    for y in 0..N {
        for x in 0..N {
            out[y][N - x - 1] = image[y][x];
        }
    }
    out
}

fn add_rule<const N: usize, const M: usize>(
    input: &str, output: &str, rules: &mut HashMap<[[u8; N]; N], [[u8; M]; M]>,
) {
    let mut key = [[0; N]; N];
    let mut value = [[0; M]; M];
    for (y, row) in input.split('/').enumerate() {
        for (x, cell) in row.bytes().enumerate() {
            key[y][x] = cell;
        }
    }
    for (y, row) in output.split('/').enumerate() {
        for (x, cell) in row.bytes().enumerate() {
            value[y][x] = cell;
        }
    }
    for _ in 0..4 {
        rules.insert(key, value);
        rules.insert(flip(key), value);
        key = rotate(key);
    }
}

pub struct Day21;

impl Day for Day21 {

    const PART1: Solution = Solution::U32(203);
    const PART2: Solution = Solution::U32(3342470);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let mut rules2 = HashMap::new();
        let mut rules3 = HashMap::new();
        for line in input.lines() {
            let (input, output) = line.split_once(" => ").unwrap();
            let len = input.chars().position(|c| c == '/').unwrap();
            if len == 2 {
                add_rule::<2, 3>(input, output, &mut rules2);
            } else if len == 3 {
                add_rule::<3, 4>(input, output, &mut rules3);
            } else {
                unreachable!();
            }
        }

        let solve = |iter_count| {
            let mut image = vec![b'.', b'#', b'.', b'.', b'.', b'#', b'#', b'#', b'#'];
            let mut image2 = Vec::new();
            let mut size = 3;
            for _ in 0..iter_count {
                size = if size % 2 == 0 {
                    let size_new = size / 2 * 3;
                    image2.resize(size_new * size_new, b' ');
                    for y in 0..size / 2 {
                        for x in 0..size / 2 {
                            let part2 = [[
                                image[x * 2     +  y * 2      * size],
                                image[x * 2 + 1 +  y * 2      * size],
                            ], [
                                image[x * 2     + (y * 2 + 1) * size],
                                image[x * 2 + 1 + (y * 2 + 1) * size],
                            ]];
                            let part3 = rules2.get(&part2).unwrap();
                            for y2 in 0..3 {
                                for x2 in 0..3 {
                                    image2[x * 3 + x2 + (y * 3 + y2) * size_new] = part3[y2][x2];
                                }
                            }
                        }
                    }
                    size_new
                } else {
                    let size_new = size / 3 * 4;
                    image2.resize(size_new * size_new, b' ');
                    for y in 0..size / 3 {
                        for x in 0..size / 3 {
                            let part3 = [[
                                image[x * 3     +  y * 3      * size],
                                image[x * 3 + 1 +  y * 3      * size],
                                image[x * 3 + 2 +  y * 3      * size],
                            ], [
                                image[x * 3     + (y * 3 + 1) * size],
                                image[x * 3 + 1 + (y * 3 + 1) * size],
                                image[x * 3 + 2 + (y * 3 + 1) * size],
                            ], [
                                image[x * 3     + (y * 3 + 2) * size],
                                image[x * 3 + 1 + (y * 3 + 2) * size],
                                image[x * 3 + 2 + (y * 3 + 2) * size],
                            ]];
                            let part4 = rules3.get(&part3).unwrap();
                            for y2 in 0..4 {
                                for x2 in 0..4 {
                                    image2[x * 4 + x2 + (y * 4 + y2) * size_new] = part4[y2][x2];
                                }
                            }
                        }
                    }
                    size_new
                };
                std::mem::swap(&mut image, &mut image2);
            }
            image.iter().filter(|&&c| c == b'#').count()
        };
        let sol1 = solve(if example { 2 } else { 5 });
        let sol2 = solve(if example { 2 } else { 18 });

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(12), Solution::None],
            input: "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#",
        },
    ];
}
