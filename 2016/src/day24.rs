use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;

pub struct Day24;

impl Day for Day24 {

    const PART1: Solution = Solution::U32(462);
    const PART2: Solution = Solution::U32(676);

    fn solve(input: &str) -> [Solution; 2] {
        let w = input.bytes().position(|ch| ch == b'\n').unwrap() + 1;
        let mut points = input.bytes().enumerate().filter_map(|(pos, ch)| {
            if ch.is_ascii_digit() {
                Some((ch - b'0', pos as i32))
            } else {
                None
            }
        }).collect::<Vec<_>>();
        points.sort_unstable_by_key(|(num, _)| *num);

        let mut dists = vec![vec![u32::MAX; points.len()]; points.len()];
        for &(start_num, start_pos) in points.iter() {
            let mut map = input.as_bytes().to_vec();
            let mut queue = VecDeque::new();
            queue.push_back((start_pos, 0));
            while let Some((pos, depth)) = queue.pop_front() {
                let ch = map[pos as usize];
                if ch == b'#' {
                    continue;
                }
                map[pos as usize] = b'#';
                if ch.is_ascii_digit() {
                    dists[start_num as usize][(ch - b'0') as usize] = depth;
                }
                for dir in [1, -1, w as i32, -(w as i32)] {
                    queue.push_back((pos + dir, depth + 1));
                }
            }
        }

        fn shortest<const R: bool>(curr: u8, dists: &[Vec<u32>], to_visit: &mut [u8]) -> u32 {
            match to_visit.len() {
                0 => return 0,
                1 => {
                    let return_dist = if R {
                        dists[to_visit[0] as usize][0]
                    } else {
                        0
                    };
                    return dists[curr as usize][to_visit[0] as usize] + return_dist;
                },
                _ => {}
            }
            let last_idx = to_visit.len() - 1;
            let mut best = u32::MAX;
            for i in 0..to_visit.len() {
                let next = to_visit[i];
                let mut dist = dists[curr as usize][next as usize];
                to_visit.swap(i, last_idx);
                dist += shortest::<R>(next, dists, &mut to_visit[..last_idx]);
                to_visit.swap(i, last_idx);
                best = best.min(dist);
            }
            best
        }

        let mut to_visit = (1..points.len() as u8).collect::<Vec<_>>();
        let sol1 = shortest::<false>(0, &dists, &mut to_visit);
        let sol2 = shortest::<true>(0, &dists, &mut to_visit);

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(14), Solution::U32(20)],
            input: "###########
#0.1.....2#
#.#######.#
#4.......3#
###########",
        },
    ];
}
