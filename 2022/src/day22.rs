use crate::Solution;
use crate::utils::iter_parse_u32;
use std::mem;

pub const SOLUTION: Solution = Solution::U32((197160, 0));

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
            other => panic!("unexpcted char {}", other),
        }
    }
    
    let mut path = Vec::new();
    while let (Some(num), Some(dir)) = iter_parse_u32(&mut iter) {
        let dir = match dir {
            b'R' => 1,
            b'L' => 3,
            b'\n' => 0,
            _ => unreachable!(),
        };
        path.push((num, dir));
    }
    
    let mut pos = (map[0].0, 0);
    let mut dir = 0;
    for &(num, d) in path.iter() {
        for _ in 0..num {
            let next = match dir {
                0 => {
                    if pos.0 == map[pos.1].0 + map[pos.1].1.len() - 1 {
                        (map[pos.1].0, pos.1)
                    } else {
                        (pos.0 + 1, pos.1)
                    }
                }
                1 => {
                    if pos.1 == map.len() - 1 || pos.0 < map[pos.1 + 1].0 || pos.0 >= map[pos.1 + 1].0 + map[pos.1 + 1].1.len() {
                        let mut y = pos.1;
                        while y > 0 && pos.0 >= map[y - 1].0 && pos.0 < map[y - 1].0 + map[y - 1].1.len() {
                            y -= 1;
                        }
                        (pos.0, y)
                    } else {
                        (pos.0, pos.1 + 1)
                    }
                }
                2 => {
                    if pos.0 == map[pos.1].0 {
                        (pos.0 + map[pos.1].1.len() - 1, pos.1)
                    } else {
                        (pos.0 - 1, pos.1)
                    }
                }
                3 => {
                    if pos.1 == 0 || pos.0 < map[pos.1 - 1].0 || pos.0 >= map[pos.1 - 1].0 + map[pos.1 - 1].1.len() {
                        let mut y = pos.1;
                        while y < map.len() - 1 && pos.0 >= map[y + 1].0 && pos.0 < map[y + 1].0 + map[y + 1].1.len() {
                            y += 1;
                        }
                        (pos.0, y)
                    } else {
                        (pos.0, pos.1 - 1)
                    }
                }
                _ => unreachable!(),
            };
            let offset = map[next.1].0;
            if map[next.1].1[next.0 - offset] != 1 {
                // map[next.1].1[next.0 - offset] = 2;
                pos = next;
            } else {
                break;
            }
        }
        dir = (dir + d) % 4;
    }
    let res1 = (pos.1 + 1) * 1000 + (pos.0 + 1) * 4 + dir;
    
    // for row in map.iter() {
        // println!("");
        // for _ in 0..row.0 {
            // print!(" ");
        // }
        // for &x in row.1.iter() {
            // match x {
                // 0 => print!("."),
                // 1 => print!("#"),
                // 2 => print!("+"),
                // _ => print!("?"),
            // }
        // }
    // }
    // println!("\n{:?}", path);
    
    (res1 as u32, 0u32).into()
}
