use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day06;

impl Day for Day06 {

    const PART1: Solution = Solution::U32(158090);
    const PART2: Solution = Solution::U32(241);

    fn solve(input: &str) -> [Solution; 2] {
        let mut orbits = Vec::new();
        let mut orbits_map = HashMap::new();
        for line in input.as_bytes().split(|c| *c == b'\n') {
            let mut parts = line.split(|c| *c == b')');
            let orbited = parts.next().unwrap();
            let orbiter = parts.next().unwrap();
            debug_assert!(parts.next().is_none());
            let orbited = *orbits_map.entry(orbited).or_insert_with(|| {
                orbits.push((None, 0));
                orbits.len() - 1
            });
            let orbiter = *orbits_map.entry(orbiter).or_insert_with(|| {
                orbits.push((None, 0));
                orbits.len() - 1
            });
            orbits[orbiter] = (Some(orbited), 0);
        }

        fn calc_indirect_orbits(orbits: &mut [(Option<usize>, u32)], idx: usize) -> u32 {
            if let Some(prev) = orbits[idx].0 {
                if orbits[idx].1 == 0 {
                    orbits[idx].1 = calc_indirect_orbits(orbits, prev) + 1;
                }
            }
            orbits[idx].1
        }

        for i in 0..orbits.len() {
            calc_indirect_orbits(&mut orbits, i);
        }
        let sol1 = orbits.iter().map(|(_, count)| *count).sum::<u32>();

        let sol2 = if let Some(you_idx) = orbits_map.get(b"YOU".as_slice()) {
            let mut you_path = vec![*you_idx];
            while let Some(idx) = orbits[*you_path.last().unwrap()].0 {
                you_path.push(idx);
            }

            let mut san_path = vec![*orbits_map.get(b"SAN".as_slice()).unwrap()];
            while let Some(idx) = orbits[*san_path.last().unwrap()].0 {
                san_path.push(idx);
            }

            let mut i = 1;
            while you_path[you_path.len() - i] == san_path[san_path.len() - i] {
                i += 1;
            }
            (you_path.len() - i + san_path.len() - i) as u32
        } else {
            0
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(42), Solution::None],
            input: "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L",
        },
        Example {
            solution: [Solution::None, Solution::U32(4)],
            input: "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN",
        },
    ];
}
