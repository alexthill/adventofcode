use aoc_lib_rust::{next_parse, Day, Example, Solution};
use aoc_lib_rust::utils::vector::Vector;
use std::collections::VecDeque;

type IVec4 = Vector<i32, 4>;

pub struct Day25;

impl Day for Day25 {

    const PART1: Solution = Solution::U32(363);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        let mut points = input.lines().map(|line| {
            let mut parts = line.trim_start().split(',');
            IVec4::from([
                next_parse!(parts, i32),
                next_parse!(parts, i32),
                next_parse!(parts, i32),
                next_parse!(parts, i32),
            ])
        }).collect::<Vec<_>>();

        let mut sol1 = 0;
        let mut queue = VecDeque::new();
        while let Some(point) = points.pop() {
            sol1 += 1;
            queue.push_back(point);
            while let Some(point) = queue.pop_front() {
                points.retain(|&other| {
                    if (point - other).map(|x| x.abs()).sum() > 3 {
                        true
                    } else {
                        queue.push_back(other);
                        false
                    }
                });
            }
        }

        [Solution::U32(sol1), Solution::None]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: " 0,0,0,0
 3,0,0,0
 0,3,0,0
 0,0,3,0
 0,0,0,3
 0,0,0,6
 9,0,0,0
12,0,0,0
",
        },
        Example {
            solution: [Solution::U32(4), Solution::None],
            input: "-1,2,2,0
0,0,2,-2
0,0,0,-2
-1,2,0,0
-2,-2,-2,2
3,0,2,-1
-1,3,2,2
-1,0,-1,0
0,2,1,-2
3,0,0,0",
        },
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "1,-1,0,1
2,0,-1,0
3,2,-1,0
0,0,3,1
0,0,-1,-1
2,3,-2,0
-2,2,0,0
2,-2,0,-1
1,-1,0,-1
3,2,0,2",
        },
        Example {
            solution: [Solution::U32(8), Solution::None],
            input: "1,-1,-1,-2
-2,-2,0,1
0,2,1,3
-2,3,-2,1
0,2,3,-2
-1,-1,1,-2
0,-2,-1,0
-2,2,3,-1
1,2,2,0
-1,-2,0,-2",
        },
    ];
}
