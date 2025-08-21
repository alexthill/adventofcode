use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next, next_parse};

struct Node {
    adj: Box<[u16]>,
    visited: bool,
}

pub struct Day12;

impl Day for Day12 {

    const PART1: Solution = Solution::U32(113);
    const PART2: Solution = Solution::U32(202);

    fn solve(input: &str) -> [Solution; 2] {
        let mut nodes = input.lines().enumerate().map(|(i, line)| {
            let mut parts = line.split(" <-> ");
            assert_eq!(next_parse!(parts, usize), i);
            Node {
                adj: next!(parts).split(", ").map(|s| s.parse().unwrap()).collect(),
                visited: false,
            }
        }).collect::<Vec<_>>();
        let mut sol1 = 0;
        let mut sol2 = 0;

        loop {
            let Some(start) = nodes.iter().position(|node| !node.visited) else {
                break;
            };
            sol2 += 1;
            let mut size = 0;
            let mut stack = vec![start as u16];
            while let Some(idx) = stack.pop() {
                if nodes[idx as usize].visited {
                    continue;
                }
                size += 1;
                nodes[idx as usize].visited = true;
                stack.extend(nodes[idx as usize].adj.iter().copied());
            }
            if sol1 == 0 {
                sol1 = size;
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(6), Solution::U32(2)],
            input: "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5",
        },
    ];
}
