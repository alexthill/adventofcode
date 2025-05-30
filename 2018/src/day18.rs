use aoc_lib_rust::{Day, Example, Solution};
use std::collections::hash_map::{HashMap, RandomState};
use std::hash::{BuildHasher, Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Cell {
    Open,
    Trees,
    Lumberyard,
    Newline,
}

pub struct Day18;

impl Day for Day18 {

    const PART1: Solution = Solution::U32(427961);
    const PART2: Solution = Solution::U32(103970);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let w = input.bytes().position(|c| c == b'\n').unwrap() as i32 + 1;
        let mut map = Vec::with_capacity(input.len());
        for c in input.bytes() {
            match c {
                b'.' => map.push(Cell::Open),
                b'|' => map.push(Cell::Trees),
                b'#' => map.push(Cell::Lumberyard),
                b'\n' => map.push(Cell::Newline),
                _ => unreachable!(),
            }
        }
        let mut map_copy = map.clone();

        fn map_count_surounding(map: &[Cell], w: i32, pos: i32, cell: Cell) -> usize {
            let dirs = [-1, 1, -w - 1, -w, -w + 1, w - 1, w, w + 1];
            dirs.into_iter().filter(|dir| {
                let pos = pos + *dir;
                map.get(pos as usize).map(|c| *c == cell).unwrap_or(false)
            }).count()
        }

        fn map_any_surounding(map: &[Cell], w: i32, pos: i32, cell: Cell) -> bool {
            let dirs = [-1, 1, -w - 1, -w, -w + 1, w - 1, w, w + 1];
            dirs.into_iter().any(|dir| {
                let pos = pos + dir;
                map.get(pos as usize).map(|c| *c == cell).unwrap_or(false)
            })
        }


        fn exec(map: &mut Vec<Cell>, map_copy: &mut Vec<Cell>, w: i32) {
            for (pos, (c, c_next)) in map.iter().zip(map_copy.iter_mut()).enumerate() {
                let pos = pos as i32;
                match c {
                    Cell::Open => {
                        if map_count_surounding(map, w, pos, Cell::Trees) >= 3 {
                            *c_next = Cell::Trees;
                        } else {
                            *c_next = Cell::Open;
                        }
                    }
                    Cell::Trees => {
                        if map_count_surounding(map, w, pos, Cell::Lumberyard) >= 3 {
                            *c_next = Cell::Lumberyard;
                        } else {
                            *c_next = Cell::Trees;
                        }
                    }
                    Cell::Lumberyard => {
                        let trees = map_any_surounding(map, w, pos, Cell::Trees);
                        let lumber = map_any_surounding(map, w, pos, Cell::Lumberyard);
                        if  !trees || !lumber {
                            *c_next = Cell::Open;
                        } else {
                            *c_next = Cell::Lumberyard;
                        }
                    }
                    Cell::Newline => {}
                }
            }
            std::mem::swap(map, map_copy);
        }

        fn get_value(map: &[Cell]) -> u32 {
            let trees = map.iter().filter(|c| **c == Cell::Trees).count();
            let lumber = map.iter().filter(|c| **c == Cell::Lumberyard).count();
            (trees * lumber) as _
        }

        for _ in 0..10 {
            exec(&mut map, &mut map_copy, w);
        }
        let sol1 = get_value(&map);
        let sol2 = if !example {
            let mut seen = HashMap::new();
            let mut seen_list = Vec::new();
            let state = RandomState::new();
            let mut sol2 = 0;
            for i in 0.. {
                exec(&mut map, &mut map_copy, w);
                let hash: u64 = state.hash_one(&map);
                match seen.get(&hash).copied() {
                    Some(old_i) => {
                        let period_len = i - old_i;
                        let modulus = (1000000000 - 10 - old_i) % period_len;
                        let idx = old_i + modulus - 1;
                        sol2 = seen_list[idx as usize];
                        break;
                    }
                    None => {
                        let value = get_value(&map);
                        seen.insert(hash, i);
                        seen_list.push(value);
                    }
                }
                if i == 10000 {
                    panic!("found no repetition after 10000 iterations");
                }
            }
            Solution::U32(sol2)
        } else {
            Solution::None
        };

        [Solution::U32(sol1), sol2]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1147), Solution::None],
            input: ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.",
        },
    ];
}
