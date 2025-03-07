use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashSet, VecDeque};

pub struct Day22;

impl Day for Day22 {

    const PART1: Solution = Solution::U32(33400);
    const PART2: Solution = Solution::U32(33745);

    fn solve(input: &str) -> [Solution; 2] {
        let mut p1 = VecDeque::new();
        let mut p2 = VecDeque::new();
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        lines.next().unwrap();
        while let Some(line) = lines.next() {
            if line.is_empty() {
                break;
            }
            p1.push_front(line.iter().fold(0, |acc, c| acc * 10 + (c - b'0') as u8));
        }
        lines.next().unwrap();
        while let Some(line) = lines.next() {
            if line.is_empty() {
                break;
            }
            p2.push_front(line.iter().fold(0, |acc, c| acc * 10 + (c - b'0') as u8));
        }

        let state = [p1.clone(), p2.clone()];
        let winner = loop {
            let (Some(c1), Some(c2)) = (p1.pop_back(), p2.pop_back()) else {
                unreachable!();
            };
            if c1 > c2 {
                p1.push_front(c1);
                p1.push_front(c2);
                if p2.is_empty() { break &p1; }
            } else {
                p2.push_front(c2);
                p2.push_front(c1);
                if p1.is_empty() { break &p2; }
            }
        };
        let sol1 = winner.iter().enumerate().map(|(i, c)| *c as u32 * (i as u32 + 1)).sum();

        fn clone_deque_range(d: &VecDeque<u8>, len: usize) -> VecDeque<u8> {
            d.range(d.len() - len..d.len()).copied().collect()
        }

        fn recursive_combat(mut state: [VecDeque<u8>; 2]) -> (usize, [VecDeque<u8>; 2]) {
            let mut states = HashSet::new();
            let winner = loop {
                if states.contains(&state) {
                    break 0;
                }
                states.insert(state.clone());
                let (Some(c0), Some(c1)) = (state[0].pop_back(), state[1].pop_back()) else {
                    unreachable!();
                };
                let cs = [c0, c1];
                let winner = if state[0].len() >= c0 as usize && state[1].len() >= c1 as usize {
                    let state = [
                        clone_deque_range(&state[0], c0 as usize),
                        clone_deque_range(&state[1], c1 as usize),
                    ];
                    recursive_combat(state).0
                } else if c0 > c1 {
                    0
                } else {
                    1
                };
                state[winner].push_front(cs[winner]);
                state[winner].push_front(cs[winner ^ 1]);
                if state[winner ^ 1].is_empty() {
                    break winner;
                }
            };
            (winner, state)
        }
        let (winner, state) = recursive_combat(state);
        let sol2 = state[winner].iter().enumerate().map(|(i, c)| *c as u32 * (i as u32 + 1)).sum();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(306), Solution::U32(291)],
            input: "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10",
        },
    ];
}
