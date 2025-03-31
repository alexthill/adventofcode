use aoc_lib_rust::{utils::lcm, Day, Example, Solution};
use std::collections::HashSet;

pub struct Day12;

impl Day for Day12 {

    const PART1: Solution = Solution::U32(8742);
    const PART2: Solution = Solution::U64(325433763467176);

    fn solve2(input: &str, is_example: bool) -> [Solution; 2] {
        let steps = if is_example { 10 } else { 1000 };
        let mut lines = input.lines().map(|line| {
            let mut parts = line.split(['=', ',', '>']).filter_map(|s| s.parse::<i32>().ok());
            (
                [parts.next().unwrap(), parts.next().unwrap(), parts.next().unwrap()],
                [0_i32, 0, 0],
            )
        });
        let mut moons = [
            lines.next().unwrap(),
            lines.next().unwrap(),
            lines.next().unwrap(),
            lines.next().unwrap(),
        ];
        debug_assert!(lines.next().is_none());

        let mut states = [
            (HashSet::new(), None),
            (HashSet::new(), None),
            (HashSet::new(), None),
        ];
        let mut sol1 = 0;
        for i in 0.. {
            if i == steps {
                sol1 = moons.iter().map(|moon| {
                    moon.0.iter().map(|pos| pos.unsigned_abs()).sum::<u32>() *
                        moon.1.iter().map(|pos| pos.unsigned_abs()).sum::<u32>()
                }).sum::<u32>();
            }

            for (x, state) in states.iter_mut().enumerate() {
                if state.1.is_some() {
                    continue;
                }

                let key = moons.map(|moon| (moon.0[x], moon.1[x]));
                if !state.0.insert(key) {
                    state.1 = Some(i);
                    continue;
                }

                for i in 0..3 {
                    for j in i + 1..4 {
                        match moons[i].0[x].cmp(&moons[j].0[x]) {
                            std::cmp::Ordering::Greater => {
                                moons[i].1[x] -= 1;
                                moons[j].1[x] += 1;
                            }
                            std::cmp::Ordering::Less => {
                                moons[i].1[x] += 1;
                                moons[j].1[x] -= 1;
                            }
                            _ => {}
                        }
                    }
                }
                for moon in moons.iter_mut() {
                    moon.0[x] += moon.1[x];
                }
            }

            if states.iter().all(|state| state.1.is_some()) {
                break;
            }
        }

        let sol2 = states.into_iter().fold(1, |acc, (_, num)| lcm(acc, num.unwrap()));

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(179), Solution::U32(2772)],
            input: "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>",
        },
        Example {
            solution: [Solution::U32(706), Solution::U64(4686774924)],
            input: "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>",
        },
    ];
}
