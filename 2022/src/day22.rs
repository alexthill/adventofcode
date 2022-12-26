use crate::Solution;
use crate::utils::iter_parse_u32;
use std::mem;

pub const SOLUTION: Solution = Solution::U32((197160, 145065));

pub fn solve(input: String) -> Solution {
    let mut iter = input.bytes();
    let mut map = Vec::new();
    let mut row = Vec::new();
    let mut offset: usize = 0;
    loop {
        match iter.next().unwrap() {
            b' ' => offset += 1,
            b'.' => row.push(0),
            b'#' => row.push(1),
            b'\n' => {
                if row.is_empty() {
                    break;
                } else {
                    map.push((mem::take(&mut offset), mem::take(&mut row)));
                }
            }
            other => panic!("unexpected char {}", other),
        }
    }
    
    let mut path = Vec::new();
    while let (Some(num), Some(dir)) = iter_parse_u32(&mut iter) {
        let dir = match dir {
            b'R' => 1,
            b'L' => 3,
            b'\n' => 0,
            other => panic!("unexpected char {}", other),
        };
        path.push((num, dir));
    }
    
    let (mut x, mut y) = (map[0].0, 0);
    let mut dir = 0;
    for &(num, d) in path.iter() {
        for _ in 0..num {
            let next = match dir {
                0 => {
                    if x == map[y].0 + map[y].1.len() - 1 {
                        (map[y].0, y)
                    } else {
                        (x + 1, y)
                    }
                }
                1 => {
                    if y == map.len() - 1 || x < map[y + 1].0 || x >= map[y + 1].0 + map[y + 1].1.len() {
                        let mut new_y = y;
                        while new_y > 0 && x >= map[new_y - 1].0 && x < map[new_y - 1].0 + map[new_y - 1].1.len() {
                            new_y -= 1;
                        }
                        (x, new_y)
                    } else {
                        (x, y + 1)
                    }
                }
                2 => {
                    if x == map[y].0 {
                        (x + map[y].1.len() - 1, y)
                    } else {
                        (x - 1, y)
                    }
                }
                3 => {
                    if y == 0 || x < map[y - 1].0 || x >= map[y - 1].0 + map[y - 1].1.len() {
                        let mut new_y = y;
                        while new_y < map.len() - 1 && x >= map[new_y + 1].0 && x < map[new_y + 1].0 + map[new_y + 1].1.len() {
                            new_y += 1;
                        }
                        (x, new_y)
                    } else {
                        (x, y - 1)
                    }
                }
                _ => unreachable!(),
            };
            let offset = map[next.1].0;
            if map[next.1].1[next.0 - offset] != 1 {
                x = next.0;
                y = next.1;
            } else {
                break;
            }
        }
        dir = (dir + d) % 4;
    }
    let res1 = (y + 1) * 1000 + (x + 1) * 4 + dir;
    
    let jumps = [
        [(0, 2), (1, 2), (0, 0), (1, 1)],
        [(3, 2), (0, 3), (0, 1), (0, 0)],
        [(2, 2), (3, 0), (2, 0), (3, 1)],
        [(2, 1), (2, 3), (1, 0), (0, 0)],
    ];
    
    let (mut x, mut y) = (map[0].0, 0);
    let mut dir = 0;
    for &(num, d) in path.iter() {
        for _ in 0..num {
            let next = match dir {
                0 => {
                    if x == map[y].0 + map[y].1.len() - 1 {
                        let jump = jumps[0][y / 50];
                        if jump.0 == 0 {
                            let new_y = 50 * jump.1 + 49 - y % 50;
                            (map[new_y].0 + map[new_y].1.len() - 1, new_y, 2)
                        } else if jump.0 == 1 {
                            let new_y = y - y % 50 - 1;
                            (50 * jump.1 + y % 50, new_y, 3)
                        } else {
                            unreachable!();
                        }
                    } else {
                        (x + 1, y, dir)
                    }
                }
                1 => {
                    if y == map.len() - 1 || x < map[y + 1].0 || x >= map[y + 1].0 + map[y + 1].1.len() {
                        let jump = jumps[1][x / 50];
                        if jump.0 == 0 {
                            let new_y = 50 * jump.1 + x % 50;
                            (map[new_y].0 + map[new_y].1.len() - 1, new_y, 2)
                        } else if jump.0 == 3 {
                            (x + 100, 0, 1)
                        } else {
                            unreachable!();
                        }
                    } else {
                        (x, y + 1, dir)
                    }
                }
                2 => {
                    if x == map[y].0 {
                        let jump = jumps[2][y / 50];
                        if jump.0 == 2 {
                            let new_y = 50 * jump.1 + 49 - y % 50;
                            (map[new_y].0, new_y, 0)
                        } else if jump.0 == 3 {
                            let new_y = (y + (50 - y % 50)) % 200;
                            (50 * jump.1 + y % 50, new_y, 1)
                        } else {
                            unreachable!();
                        }
                    } else {
                        (x - 1, y, dir)
                    }
                }
                3 => {
                    if y == 0 || x < map[y - 1].0 || x >= map[y - 1].0 + map[y - 1].1.len() {
                        let jump = jumps[3][x / 50];
                        if jump.0 == 2 {
                            let new_y = 50 * jump.1 + x % 50;
                            (map[new_y].0 + map[new_y].1.len() - 1, new_y, 0)
                        } else if jump.0 == 1 {
                            (x - 100, 199, 3)
                        } else {
                            unreachable!();
                        }
                    } else {
                        (x, y - 1, dir)
                    }
                }
                _ => unreachable!(),
            };
            let offset = map[next.1].0;
            if map[next.1].1[next.0 - offset] != 1 {
                map[next.1].1[next.0 - offset] = next.2 + 2;
                x = next.0;
                y = next.1;
                dir = next.2;
            } else {
                break;
            }
        }
        dir = (dir + d) % 4;
    }
    let res2 = (y + 1) * 1000 + (x + 1) * 4 + dir;
    
    (res1 as u32, res2 as u32).into()
}
