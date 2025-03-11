use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashMap, HashSet};

pub struct Day24;

impl Day for Day24 {

    const PART1: Solution = Solution::U32(230);
    const PART2: Solution = Solution::U32(3565);

    fn solve(input: &str) -> [Solution; 2] {
        let mut black = HashSet::new();
        for line in input.as_bytes().split(|c| *c == b'\n') {
            let mut line = line.into_iter();
            let mut pos = (0, 0);
            while let Some(c) = line.next() {
                let (dx, dy) = match c {
                    b'e' => (2, 0),
                    b'w' => (-2, 0),
                    b's' => match line.next() {
                        Some(b'e') => (1, 1),
                        Some(b'w') => (-1, 1),
                        _ => unreachable!(),
                    }
                    b'n' => match line.next() {
                        Some(b'e') => (1, -1),
                        Some(b'w') => (-1, -1),
                        _ => unreachable!(),
                    }
                    _ => unreachable!(),
                };
                pos.0 += dx;
                pos.1 += dy;
            }
            if black.contains(&pos) {
                black.remove(&pos);
            } else {
                black.insert(pos);
            }
        }
        let sol1 = black.len() as u32;

        let mut counts = HashMap::new();
        for _ in 0..100 {
            for (x, y) in black.iter().copied() {
                counts.entry((x, y)).or_insert(0);
                for (dx, dy) in [(-2, 0), (2, 0), (-1, -1), (1, -1), (-1, 1), (1, 1)] {
                    *counts.entry((x + dx, y + dy)).or_insert(0) += 1;
                }
            }
            for (pos, &count) in counts.iter() {
                if black.contains(pos) {
                    if count == 0 || count > 2 {
                        black.remove(pos);
                    }
                } else if count == 2 {
                    black.insert(*pos);
                }
            }
            counts.clear();
        }
        let sol2 = black.len() as u32;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1), Solution::U32(0)],
            input: "esew",
        },
        Example {
            solution: [Solution::U32(0), Solution::None],
            input: "nwwswee
nwwswee",
        },
        Example {
            solution: [Solution::U32(10), Solution::U32(2208)],
            input: "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew",
        },
    ];
}
