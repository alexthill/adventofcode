use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashMap, HashSet};

pub struct Day17;

impl Day17 {
    #[allow(dead_code)]
    fn print_cube(cube: &HashSet<(i32, i32, i32)>) {
        let mut x_min = i32::MAX;
        let mut y_min = i32::MAX;
        let mut z_min = i32::MAX;
        let mut x_max = i32::MIN;
        let mut y_max = i32::MIN;
        let mut z_max = i32::MIN;
        for (x, y, z) in cube.iter().copied() {
            x_min = x_min.min(x);
            y_min = y_min.min(y);
            z_min = z_min.min(z);
            x_max = x_max.max(x);
            y_max = y_max.max(y);
            z_max = z_max.max(z);
        }

        for z in z_min..=z_max {
            println!("z = {z}");
            for y in y_min..=y_max {
                for x in x_min..=x_max {
                    if cube.contains(&(x, y, z)) {
                        print!("#");
                    } else {
                        print!(".");
                    }
                }
                println!();
            }
            println!();
        }
    }
}

impl Day for Day17 {

    const PART1: Solution = Solution::U32(209);
    const PART2: Solution = Solution::U32(1492);

    fn solve(input: &str) -> [Solution; 2] {
        let mut active3 = HashSet::new();
        let mut active4 = HashSet::new();
        for (y, line) in input.as_bytes().split(|c| *c == b'\n').enumerate() {
            for (x, c) in line.iter().enumerate() {
                if *c == b'#' {
                    active3.insert((x as i32, y as i32, 0_i32));
                    active4.insert((x as i32, y as i32, 0_i32, 0_i32));
                }
            }
        }

        let mut active_counts = HashMap::new();
        for _ in 0..6 {
            for (x, y, z) in active3.iter().copied() {
                for x in x-1..=x+1 {
                    for y in y-1..=y+1 {
                        for z in z-1..=z+1 {
                            *active_counts.entry((x, y, z)).or_insert(0) += 1;
                        }
                    }
                }
            }
            for (pos, &count) in active_counts.iter() {
                if active3.contains(pos) {
                    if count != 3 && count != 4 {
                        active3.remove(pos);
                    }
                } else {
                    if count == 3 {
                        active3.insert(*pos);
                    }
                }
            }
            active_counts.clear();
        }

        let mut active_counts = HashMap::new();
        for _ in 0..6 {
            for (x, y, z, w) in active4.iter().copied() {
                for x in x-1..=x+1 {
                    for y in y-1..=y+1 {
                        for z in z-1..=z+1 {
                            for w in w-1..=w+1 {
                                *active_counts.entry((x, y, z, w)).or_insert(0) += 1;
                            }
                        }
                    }
                }
            }
            for (pos, &count) in active_counts.iter() {
                if active4.contains(pos) {
                    if count != 3 && count != 4 {
                        active4.remove(pos);
                    }
                } else {
                    if count == 3 {
                        active4.insert(*pos);
                    }
                }
            }
            active_counts.clear();
        }

        [Solution::U32(active3.len() as u32), Solution::U32(active4.len() as u32)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(112), Solution::U32(848)],
            input: ".#.
..#
###",
        },
    ];
}
