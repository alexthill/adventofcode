use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use std::collections::HashMap;

pub struct Day20;

impl Day for Day20 {

    const PART1: Solution = Solution::U32(3644);
    const PART2: Solution = Solution::U32(8523);

    fn solve(input: &str) -> [Solution; 2] {
        let input = input.as_bytes();

        let mut rooms = vec![(IVec2::from([0, 0]), 0)];
        let mut map = HashMap::from([(IVec2::from([0, 0]), 0)]);
        let mut stack = vec![0];
        let mut curr = 0;

        for c in &input[1..input.len() - 1] {
            match c {
                b'(' => stack.push(curr),
                b')' => curr = stack.pop().unwrap(),
                b'|' => curr = *stack.last().unwrap(),
                other => {
                    let curr_pos = rooms[curr].0;
                    let next_pos = match other {
                        b'N' => curr_pos + [ 0, -1].into(),
                        b'S' => curr_pos + [ 0,  1].into(),
                        b'E' => curr_pos + [ 1,  0].into(),
                        b'W' => curr_pos + [-1,  0].into(),
                        _ => unreachable!(),
                    };
                    let next = *map.entry(next_pos).or_insert_with(|| {
                        rooms.push((next_pos, u32::MAX));
                        rooms.len() - 1
                    });
                    rooms[next].1 = rooms[next].1.min(rooms[curr].1 + 1);
                    curr = next;
                }
            }
        }

        let sol1 = map.values().map(|room| rooms[*room].1).max().unwrap();
        let sol2 = map.values().filter(|room| rooms[**room].1 >= 1000).count() as u32;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "^WNE$",
        },
        Example {
            solution: [Solution::U32(10), Solution::None],
            input: "^ENWWW(NEEE|SSE(EE|N))$",
        },
        Example {
            solution: [Solution::U32(18), Solution::None],
            input: "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$",
        },
        Example {
            solution: [Solution::U32(23), Solution::None],
            input: "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$",
        },
        Example {
            solution: [Solution::U32(31), Solution::None],
            input: "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$",
        },
    ];
}
