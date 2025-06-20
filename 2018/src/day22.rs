use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next, utils::vector::IVec2};
use std::cmp::Ordering;
use std::collections::BinaryHeap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Equipped {
    Torch,
    Climbing,
    Neither,
}

impl Equipped {
    fn valid(&self, ty: u32) -> bool {
        match self {
            Self::Torch => ty != 1,
            Self::Climbing => ty != 2,
            Self::Neither => ty != 0,
        }
    }

    fn other(&self, ty: u32) -> Self {
        match (self, ty) {
            (Self::Torch, 0) => Self::Climbing,
            (Self::Torch, 2) => Self::Neither,
            (Self::Climbing, 0) => Self::Torch,
            (Self::Climbing, 1) => Self::Neither,
            (Self::Neither, 1) => Self::Climbing,
            (Self::Neither, 2) => Self::Torch,
            (eq, ty) => unreachable!("{eq:?} {ty}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct State {
    pos: IVec2,
    time: u16,
    equipped: Equipped,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        // We put this in a max heap but actually need a min heap
        // therfore reverse the ordering.
        self.time.cmp(&other.time).reverse()
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct Day22;

impl Day for Day22 {

    const PART1: Solution = Solution::U32(5786);
    const PART2: Solution = Solution::U32(986);

    fn solve(input: &str) -> [Solution; 2] {
        const MOD: u32 = 20183;

        let mut lines = input.lines();
        let depth = next!(lines)["depth: ".len()..].parse::<u32>().unwrap();
        let target = next!(lines)["target: ".len()..].split_once(',').unwrap();
        let target = [target.0.parse::<u32>().unwrap(), target.1.parse::<u32>().unwrap()];
        let w = target[0] * 8 + 1;
        let h = target[1] * 2 + 1;

        let geo_to_ero = |geo| (geo + depth) % MOD;

        let mut map = vec![vec![(0, [u16::MAX; 3]); w as usize]; h as usize];
        let ero = geo_to_ero(0);
        map[0][0].0 = ero;
        for x in 1..w {
            let ero = geo_to_ero(x * 16807);
            map[0][x as usize].0 = ero;
        }
        for y in 1..h {
            let mut ero = geo_to_ero(y * 48271);
            map[y as usize][0].0 = ero;
            for x in 1..w {
                ero = geo_to_ero(ero * map[(y - 1) as usize][x as usize].0);
                map[y as usize][x as usize].0 = ero;
            }
        }
        map[target[1] as usize][target[0] as usize].0 = ero;
        let sol1 = map[..=target[1] as usize].iter().map(|row| {
            row[..=target[0] as usize].iter().map(|entry| entry.0 % 3).sum::<u32>()
        }).sum::<u32>();

        let target_pos = IVec2::from([target[0] as i32, target[1] as i32]);
        let state_state = State { time: 0, pos: IVec2::default(), equipped: Equipped::Torch };
        let mut queue = BinaryHeap::from([state_state]);
        let sol2 = loop {
            let Some(State { time, pos, equipped }) = queue.pop() else {
                panic!("target not found");
            };
            if pos == target_pos && equipped == Equipped::Torch {
                break time as u32;
            }
            map[pos.y() as usize][pos.x() as usize].1[equipped as usize] = time;

            for dir in [[1, 0], [-1, 0], [0, 1], [0, -1]] {
                let pos = pos + dir.into();
                if !(0..w as i32).contains(&pos.x()) || !(0..h as i32).contains(&pos.y()) {
                    continue;
                }
                let next = map[pos.y() as usize][pos.x() as usize];
                if !equipped.valid(next.0 % 3) || next.1[equipped as usize] <= time + 1 {
                    continue;
                }
                map[pos.y() as usize][pos.x() as usize].1[equipped as usize] = time + 1;
                let next_state = State { time: time + 1, pos, equipped };
                queue.push(next_state);
            }

            let (ty, times) = map[pos.y() as usize][pos.x() as usize];
            let other = equipped.other(ty % 3);
            if times[other as usize] > time + 7 {
                let next_state = State { time: time + 7, pos, equipped: other };
                map[pos.y() as usize][pos.x() as usize].1[other as usize] = time + 7;
                queue.push(next_state);
            }
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(114), Solution::U32(45)],
            input: "depth: 510
target: 10,10",
        },
    ];
}
