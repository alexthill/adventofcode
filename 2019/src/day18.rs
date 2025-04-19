use aoc_lib_rust::{Day, Example, Solution};
use std::collections::{HashMap, VecDeque};

pub struct Day18;

impl Day for Day18 {

    const PART1: Solution = Solution::U32(3862);
    const PART2: Solution = Solution::U32(1626);

    fn solve(input: &str) -> [Solution; 2] {
        let mut map = input.as_bytes().to_vec();
        let w = map.iter().position(|c| *c == b'\n').unwrap() + 1;
        let pos = map.iter().position(|c| *c == b'@').unwrap();

        let like_real_input = [pos + 1, pos - 1, pos + w, pos - w].into_iter()
            .all(|pos| map[pos] == b'.');

        #[derive(Debug, Clone, Copy)]
        struct Key {
            key: u8,
            dist: u32,
            pos: i32,
            doors: u32,
        }

        fn bfs(
            map: &[u8],
            w: i32,
            pos: i32,
        ) -> Vec<Key> {
            struct Entry {
                pos: i32,
                dir: i32,
                dist: u32,
                doors: u32,
            }

            let mut nodes = Vec::new();
            let mut queue = VecDeque::from([Entry { pos, dir: 0, dist: 0, doors: 0 }]);
            while let Some(entry) = queue.pop_front() {
                let dist = entry.dist + 1;
                for dir in [1, -1, w, -w].into_iter().filter(|&dir| dir != -entry.dir) {
                    let mut doors = entry.doors;
                    let pos = entry.pos + dir;
                    match map[pos as usize] {
                        b'#' => continue,
                        c if c.is_ascii_lowercase() => {
                            nodes.push(Key { key: c - b'a', dist, pos, doors });
                        }
                        c if c.is_ascii_uppercase() => {
                            doors |= 1 << (c - b'A');
                        }
                        _ => {}
                    }
                    queue.push_back(Entry { pos, dir, dist, doors });
                }
            }
            nodes
        }

        if like_real_input {
            map[pos + w] = b'#';
            map[pos - w] = b'#';
        }
        let mut keys = bfs(&map, w as i32, pos as i32);
        if like_real_input {
            map[pos] = b'#';
        }
        keys.sort_unstable_by_key(|key| key.key);

        let mut key_keys = keys.iter().map(|key| {
            if like_real_input {
                if key.pos < (map.len() / 2) as i32 {
                    map[pos + w] = b'#';
                    map[pos - w] = b'.';
                } else {
                    map[pos + w] = b'.';
                    map[pos - w] = b'#';
                }
            }
            bfs(&map, w as i32, key.pos)
        }).collect::<Vec<_>>();

        fn find_shortest_path<const N: usize>(
            cache: &mut HashMap<([usize; N], u32), u32>,
            key_keys: &[Vec<Key>],
            ats: [usize; N],
            collected: u32,
            current: u32,
        ) -> u32 {
            if collected == !(!0 << key_keys.len()) {
                return current;
            }
            if let Some(dist) = cache.get(&(ats, collected)) {
                return current.saturating_add(*dist);
            }

            let mut min = u32::MAX;
            for (i, &at) in ats.iter().enumerate() {
                for key in key_keys[at].iter() {
                    if (collected & (1 << key.key)) != 0 || (key.doors | collected) != collected {
                        continue;
                    }
                    let mut ats = ats;
                    ats[i] = key.key as usize;
                    let collected = collected | (1 << key.key);
                    let current = current + key.dist;
                    min = min.min(find_shortest_path(cache, key_keys, ats, collected, current));
                }
            }
            cache.insert((ats, collected), min - current);
            min
        }

        key_keys.push(keys);
        let mut cache = HashMap::new();
        let key_idx = key_keys.len() - 1;
        let sol1 = find_shortest_path(&mut cache, &key_keys, [key_idx], 1 << key_idx, 0);

        let sol2 = if like_real_input {
            for pos in [pos, pos + 1, pos - 1, pos + w, pos - w] {
                map[pos] = b'#';
            }

            let robo_pos = [pos + w + 1, pos + w - 1, pos - w + 1, pos - w - 1];
            let keys = key_keys.pop().unwrap();
            let keys2 = robo_pos.map(|pos| bfs(&map, w as i32, pos as i32));
            let mut key_keys = keys.iter()
                .map(|key| bfs(&map, w as i32, key.pos))
                .collect::<Vec<_>>();
            key_keys.extend(keys2);

            let mut cache = HashMap::new();
            let key_idx = [4, 3, 2, 1].map(|x| key_keys.len() - x);
            let collected = key_idx.iter().fold(0, |acc, &idx| acc | (1 << idx));
            let sol2 = find_shortest_path(&mut cache, &key_keys, key_idx, collected, 0);
            Solution::U32(sol2)
        } else {
            Solution::None
        };

        [Solution::U32(sol1), sol2]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(8), Solution::None],
            input: "#########
#b.A.@.a#
#########",
        },
        Example {
            solution: [Solution::U32(86), Solution::None],
            input: "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################",
        },
        Example {
            solution: [Solution::U32(132), Solution::None],
            input: "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################",
        },
        Example {
            solution: [Solution::U32(136), Solution::None],
            input: "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################",
        },
        Example {
            solution: [Solution::U32(81), Solution::None],
            input: "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################",
        },
        Example {
            solution: [Solution::None, Solution::U32(8)],
            input: "#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######",
        },
        Example {
            solution: [Solution::None, Solution::U32(24)],
            input: "###############
#d.ABC.#.....a#
######...######
######.@.######
######...######
#b.....#.....c#
###############",
        },
        Example {
            solution: [Solution::None, Solution::U32(32)],
            input: "#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############",
        },
        Example {
            solution: [Solution::None, Solution::U32(72)],
            input: "#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba...BcIJ#
#####.@.#####
#nK.L...G...#
#M###N#H###.#
#o#m..#i#jk.#
#############",
        },
    ];
}
