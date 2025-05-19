use aoc_lib_rust::{Day, Example, Solution};
use std::ops::Range;

pub struct Day11;

impl Day for Day11 {

    const PART1: Solution = Solution::Str("33,54");
    const PART2: Solution = Solution::Str("232,289,8");

    fn solve(input: &str) -> [Solution; 2] {
        let grid_num = input.parse::<i32>().unwrap();
        let mut grid = [[0_i32; 300]; 300];
        for y in 1..=300 {
            for x in 1..=300 {
                let rack_id = x + 10;
                let begin = rack_id * y;
                let increase = begin + grid_num;
                let level = ((increase * rack_id) % 1000) / 100 - 5;
                grid[y as usize - 1][x as usize - 1] = level;
            }
        }

        fn get_max_square(grid: &[[i32; 300]; 300], sizes: Range<usize>) -> [usize; 3] {
            let mut max = 0;
            let mut res = [0; 3];
            for size in sizes {
                for y in 0..=300 - size {
                    let mut sum = 0;
                    for y2 in y..y + size {
                        for x2 in 0..size {
                            sum += grid[y2][x2];
                        }
                    }
                    if sum > max {
                        max = sum;
                        res = [0, y, size];
                    }

                    for x in 0..300 - size {
                        for y2 in y..y + size {
                            sum -= grid[y2][x];
                            sum += grid[y2][x + size];
                        }
                        if sum > max {
                            max = sum;
                            res = [x + 1, y, size];
                        }
                    }
                }
            }
            res
        }

        let [x, y, _] = get_max_square(&grid, 3..4);
        let sol1 = format!("{},{}", x + 1, y + 1);

        // just guess some range in which the best block size probably is, to make this faster
        let [x, y, size] = get_max_square(&grid, 5..20);
        let sol2 = format!("{},{},{size}", x + 1, y + 1);

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("33,45"), Solution::Str("90,269,16")],
            input: "18",
        },
        Example {
            solution: [Solution::Str("21,61"), Solution::Str("232,251,12")],
            input: "42",
        },
    ];
}
