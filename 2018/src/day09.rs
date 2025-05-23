use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;

pub struct Day09;

impl Day for Day09 {

    const PART1: Solution = Solution::U32(412959);
    const PART2: Solution = Solution::U32(3333662986);

    fn solve(input: &str) -> [Solution; 2] {
        let mut iter = input.split(' ');
        let player_count = iter.next().unwrap().parse::<usize>().unwrap();
        let last_marble = iter.nth(5).unwrap().parse::<u32>().unwrap();

        let mut list = VecDeque::from([0]);
        let mut players = vec![0; player_count];
        for i in 1..=last_marble {
            if i % 23 == 0 {
                for _ in 0..7 {
                    let el = list.pop_back().unwrap();
                    list.push_front(el);
                }
                players[i as usize % player_count] += i + list.pop_back().unwrap();
                let el = list.pop_front().unwrap();
                list.push_back(el);
            } else {
                let el = list.pop_front().unwrap();
                list.push_back(el);
                list.push_back(i);
            }
        }
        let sol1 = *players.iter().max().unwrap();

        for i in last_marble + 1..=last_marble * 100 {
            if i % 23 == 0 {
                for _ in 0..7 {
                    let el = list.pop_back().unwrap();
                    list.push_front(el);
                }
                players[i as usize % player_count] += i + list.pop_back().unwrap();
                let el = list.pop_front().unwrap();
                list.push_back(el);
            } else {
                let el = list.pop_front().unwrap();
                list.push_back(el);
                list.push_back(i);
            }
        }
        let sol2 = *players.iter().max().unwrap();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(32), Solution::None],
            input: "10 players; last marble is worth 25 points",
        },
        Example {
            solution: [Solution::U32(8317), Solution::None],
            input: "10 players; last marble is worth 1618 points",
        },
        Example {
            solution: [Solution::U32(146373), Solution::None],
            input: "13 players; last marble is worth 7999 points",
        },
    ];
}
