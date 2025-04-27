use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;

pub struct Day20;

impl Day for Day20 {

    const PART1: Solution = Solution::U32(442);
    const PART2: Solution = Solution::U32(5208);

    fn solve(input: &str) -> [Solution; 2] {
        let mut map = input.as_bytes().to_vec();
        let len = map.len();
        let w = map.iter().position(|c| *c == b'\n').unwrap() + 1;
        let h = len / w;

        let mut portals = [(0, 0); 26 * 26];
        for (pos, &c) in map.iter().enumerate().filter(|(_, c)| c.is_ascii_uppercase()) {
            for dir in [1, w] {
                if pos + dir < len && map[pos + dir].is_ascii_uppercase() {
                    let id = (c - b'A') as usize * 26 + (map[pos + dir] - b'A') as usize;
                    let ppos = if pos >= dir && map[pos - dir] == b'.' {
                        pos - dir
                    } else if pos + 2 * dir < len && map[pos + 2 * dir] == b'.' {
                        pos + 2 * dir
                    } else {
                        unreachable!()
                    };
                    if portals[id].0 == 0 {
                        portals[id].0 = ppos;
                    } else {
                        portals[id].1 = ppos;
                    }
                }
            }
        }

        let mut sol1 = 0;
        let mut queue = VecDeque::from([(portals[0].0, 0)]);
        'outer: while let Some((pos, dist)) = queue.pop_front() {
            let dist = dist + 1;
            for dir in [1, -1, w as isize, -(w as isize)] {
                let next = ((pos as isize) + dir) as usize;
                match map[next] {
                    b'.' => {
                        map[next] = b'*';
                        queue.push_back((next, dist));
                    }
                    c if c.is_ascii_uppercase() => {
                        let next2 = ((next as isize) + dir) as usize;
                        let id = if dir > 0 {
                            (map[next] - b'A') as usize * 26 + (map[next2] - b'A') as usize
                        } else {
                            (map[next2] - b'A') as usize * 26 + (map[next] - b'A') as usize
                        };
                        map[next] = b'*';
                        if id == portals.len() - 1 {
                            sol1 = dist - 1;
                            break 'outer;
                        }
                        if id != 0 {
                            if portals[id].0 == pos as usize {
                                queue.push_back((portals[id].1, dist));
                            } else if portals[id].1 == pos as usize {
                                queue.push_back((portals[id].0, dist));
                            } else {
                                unreachable!();
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut sol2 = 0;
        let mut maps = vec![input.as_bytes().to_vec()];
        let mut queue = VecDeque::from([(portals[0].0, 0, 0)]);
        'outer: while let Some((pos, dist, level)) = queue.pop_front() {
            if level > 32 {
                continue;
            }
            if level == maps.len() {
                maps.push(input.as_bytes().to_vec());
            }
            let dist = dist + 1;
            let map = &mut maps[level];
            for dir in [1, -1, w as isize, -(w as isize)] {
                let next = ((pos as isize) + dir) as usize;
                match map[next] {
                    b'.' => {
                        map[next] = b'*';
                        queue.push_back((next, dist, level));
                    }
                    c if c.is_ascii_uppercase() => {
                        let next2 = ((next as isize) + dir) as usize;
                        let id = if dir > 0 {
                            (map[next] - b'A') as usize * 26 + (map[next2] - b'A') as usize
                        } else {
                            (map[next2] - b'A') as usize * 26 + (map[next] - b'A') as usize
                        };
                        map[next] = b'*';
                        if id == portals.len() - 1 {
                            if level == 0 {
                                sol2 = dist - 1;
                                break 'outer;
                            }
                        } else if id != 0 {
                            let (x, y) = (next % w, next / w);
                            let level = if (2..w-3).contains(&x) && (2..h-2).contains(&y) {
                                level + 1
                            } else if level != 0 {
                                level - 1
                            } else {
                                continue;
                            };
                            if portals[id].0 == pos as usize {
                                queue.push_back((portals[id].1, dist, level));
                            } else if portals[id].1 == pos as usize {
                                queue.push_back((portals[id].0, dist, level));
                            } else {
                                unreachable!();
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(23), Solution::U32(26)],
            input: "         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       ",
        },
        Example {
            solution: [Solution::U32(58), Solution::None],
            input: "                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               ",
        },
        Example {
            solution: [Solution::U32(77), Solution::U32(396)],
            input: "             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     ",
        },
    ];
}
