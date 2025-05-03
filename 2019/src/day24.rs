use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashSet;

pub struct Day24;

impl Day for Day24 {

    const PART1: Solution = Solution::U32(32776479);
    const PART2: Solution = Solution::U32(2017);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let initial_state = input.bytes()
            .rev()
            .filter(|c| *c != b'\n')
            .fold(0, |acc, c| (acc << 1) | (c == b'#') as u32);

        let mut state = initial_state;
        let mut set = HashSet::new();
        let sol1 = loop {
            if !set.insert(state) {
                break (0..25).fold(0, |acc, pos| acc | (state & (1 << pos)));
            }
            state = (0..25).fold(0, |acc, pos| {
                let (x, y) = (pos % 5, pos / 5);
                let adjs = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)];
                let adj_bugs = adjs.into_iter().map(|(x, y)| {
                    if !(0..5).contains(&x) || !(0..5).contains(&y) {
                        0
                    } else {
                        (state >> (x + 5 * y)) & 1
                    }
                }).sum::<u32>();
                let old_bug = ((state >> pos) & 1) == 1;
                let new_bug = adj_bugs == 1 || (!old_bug && adj_bugs == 2);
                acc | ((new_bug as u32) << pos)
            });
        };

        let mut states = [0; 403];
        let mut states_new = [0; 403];
        let middle = states.len() / 2;
        states[middle] = initial_state;
        let (mut min_depth, mut max_depth) = (middle - 1, middle + 1);
        for _ in 0..if example { 10 } else { 200 } {
            for d in min_depth..=max_depth {
                let (state_d, state, state_u) = (states[d - 1], states[d], states[d + 1]);
                let state_new = (0..25).fold(0, |acc, pos| {
                    if pos == 12 {
                        return acc;
                    }

                    let (x, y) = (pos % 5, pos / 5);
                    let adjs = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)];
                    let adj_bugs = adjs.into_iter().map(|(x, y)| {
                        if x == -1 {
                            (state_u >> 11) & 1
                        } else if x == 5 {
                            (state_u >> 13) & 1
                        } else if y == -1 {
                            (state_u >> 7) & 1
                        } else if y == 5 {
                            (state_u >> 17) & 1
                        } else if x == 2 && y == 2 {
                            match pos {
                                 7 => (state_d & 0b00000_00000_00000_00000_11111).count_ones(),
                                11 => (state_d & 0b00001_00001_00001_00001_00001).count_ones(),
                                13 => (state_d & 0b10000_10000_10000_10000_10000).count_ones(),
                                17 => (state_d & 0b11111_00000_00000_00000_00000).count_ones(),
                                _ => unreachable!(),
                            }
                        } else {
                            (state >> (x + 5 * y)) & 1
                        }
                    }).sum::<u32>();
                    let old_bug = ((state >> pos) & 1) == 1;
                    let new_bug = adj_bugs == 1 || (!old_bug && adj_bugs == 2);
                    acc | ((new_bug as u32) << pos)
                });
                states_new[d] = state_new;
            }
            min_depth -= (states_new[min_depth] != 0) as usize;
            max_depth += (states_new[max_depth] != 0) as usize;
            std::mem::swap(&mut states, &mut states_new);
        };
        let sol2 = states.into_iter().map(|state| state.count_ones()).sum();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2129920), Solution::U32(99)],
            input: "....#
#..#.
#..##
..#..
#....",
        },
    ];
}
