use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::next_parse;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Sand,
    Clay,
    Water,
    Settled,
}

fn _print_map(map: &[Cell], w: usize) {
    for (i, c) in map.iter().enumerate() {
        let c = match c {
            Cell::Sand => '.',
            Cell::Clay => '#',
            Cell::Water => '|',
            Cell::Settled => '~',
        };
        if i % w == 0 {
            println!();
        }
        print!("{c}");
    }
    println!();
}

pub struct Day17;

impl Day for Day17 {

    const PART1: Solution = Solution::U32(34291);
    const PART2: Solution = Solution::U32(28487);

    fn solve(input: &str) -> [Solution; 2] {
        let clay = input.lines().map(|line| {
            let mut parts = line.split(['=', ',', '.']);
            let is_x = match parts.next().unwrap() {
                "x" => true,
                "y" => false,
                _ => unreachable!(),
            };
            let coord = next_parse!(parts, i32);
            assert!(parts.next().is_some());
            let from = next_parse!(parts, i32);
            assert_eq!(parts.next(), Some(""));
            let to = next_parse!(parts, i32);
            if is_x {
                (coord, coord, from, to)
            } else {
                (from, to, coord, coord)
            }
        }).collect::<Vec<_>>();

        let x_min = clay.iter().map(|clay| clay.0).min().unwrap();
        let x_max = clay.iter().map(|clay| clay.1).max().unwrap();
        let y_min = clay.iter().map(|clay| clay.2).min().unwrap();
        let y_max = clay.iter().map(|clay| clay.3).max().unwrap();

        let w = x_max - x_min + 3;
        let h = y_max - y_min + 1;

        let mut map = vec![Cell::Sand; (w * h) as usize];
        for (x1, x2, y1, y2) in clay {
            for y in y1..=y2 {
                for x in x1..=x2 {
                    map[(x - x_min + 1 + w * (y - y_min)) as usize] = Cell::Clay;
                }
            }
        }

        let w = w as usize;
        let spring_x = (500 - x_min + 1) as usize;
        let mut sources = vec![spring_x];
        'outer: while let Some(source) = sources.pop() {
            let mut pos = source;
            loop {
                map[pos] = Cell::Water;
                pos += w;
                if pos >= map.len() || map[pos] == Cell::Water {
                    continue 'outer;
                }
                if map[pos] == Cell::Clay || map[pos] == Cell::Water {
                    break;
                }
            }

            let l = (0..).take_while(|l| map[pos - l - 1] == Cell::Clay).count();
            let r = (0..).take_while(|r| map[pos + r + 1] == Cell::Clay).count();
            let mut fill_height = pos - w;
            while map[fill_height - l] == Cell::Clay && map[fill_height + r] == Cell::Clay {
                fill_height -= w;
            }

            for pos in (source.min(fill_height)..source).step_by(w) {
                map[pos] = Cell::Water;
            }
            for pos in (fill_height + w..pos).step_by(w) {
                map[pos] = Cell::Settled;
            }

            let mut added_source = false;
            while pos > fill_height || !added_source {
                pos -= w;
                for dir in [-1, 1] {
                    let mut pos2 = (pos as i32 + dir) as usize;
                    while map[pos2] == Cell::Sand {
                        if pos <= fill_height {
                            if map[pos2 + w] == Cell::Sand {
                                sources.push(pos2);
                                added_source = true;
                                break;
                            }
                            map[pos2] = Cell::Water;
                        } else {
                            map[pos2] = Cell::Settled;
                        }
                        let mut pos3 = pos2 + w;
                        while map[pos3] == Cell::Sand {
                            map[pos3] = Cell::Settled;
                            pos3 += w;
                        }
                        pos2 = (pos2 as i32 + dir) as usize;
                    }
                    if map[pos2] == Cell::Water {
                        if pos == fill_height {
                            added_source = true;
                        } else {
                            while map[pos2] == Cell::Water {
                                map[pos2] = Cell::Settled;
                                pos2 = (pos2 as i32 + dir) as usize;
                            }
                        }
                    }
                }
                if pos <= fill_height && !added_source {
                    map[pos] = Cell::Settled;
                    for dir in [-1, 1] {
                        let mut pos2 = (pos as i32 + dir) as usize;
                        while map[pos2] == Cell::Water {
                            map[pos2] = Cell::Settled;
                            pos2 = (pos2 as i32 + dir) as usize;
                        }
                    }
                }
            }
        }

        let sol1 = map.iter().filter(|&&c| c == Cell::Water || c == Cell::Settled).count();
        let sol2 = map.iter().filter(|&&c| c == Cell::Settled).count();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(57), Solution::U32(29)],
            input: "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504",
        },
        Example {
            solution: [Solution::U32(91), Solution::U32(70)],
            input: "x=499, y=5..9
y=9, x=499..501
x=501, y=5..9
x=495, y=2..12
y=12, x=495..505
x=505, y=3..12",
        },
        Example {
            solution: [Solution::U32(71), Solution::U32(53)],
            input: "x=496, y=2..9
y=9, x=495..498
x=498, y=2..9
x=494, y=4..12
y=12, x=494..504
x=504, y=3..12
x=508, y=1..2",
        },
    ];
}
