use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;

pub struct Day19;

impl Day for Day19 {

    const PART1: Solution = Solution::Str("KGPTMEJVS");
    const PART2: Solution = Solution::U32(16328);

    fn solve(input: &str) -> [Solution; 2] {
        let map = input.as_bytes();
        let w = map.iter().position(|c| *c == b'\n').unwrap() as i32 + 1;
        let start_x = map.iter().position(|c| *c == b'|').unwrap();

        let mut pos = IVec2::from([start_x as i32, 0]);
        let mut dir = IVec2::from([0, 1]);
        let mut sol1 = String::new();
        let mut sol2 = 0;
        loop {
            pos += dir;
            sol2 += 1;
            match map[(pos.x() + pos.y() * w) as usize] {
                b'|' | b'-' => continue,
                b'+' => {
                    let dir_new = IVec2::from([dir.y(), -dir.x()]);
                    let pos_new = pos + dir_new;
                    let new = map[(pos_new.x() + pos_new.y() * w) as usize];
                    if new != b' ' {
                        dir = dir_new;
                        continue;
                    }

                    let dir_new = IVec2::from([-dir.y(), dir.x()]);
                    let pos_new = pos + dir_new;
                    let new = map[(pos_new.x() + pos_new.y() * w) as usize];
                    if new != b' ' {
                        dir = dir_new;
                        continue;
                    }

                    unreachable!();
                },
                b' ' => break,
                c if c.is_ascii_alphabetic() => sol1.push(char::from_u32(c as u32).unwrap()),
                _ => unreachable!(),
            }
        }

        [Solution::String(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("ABCDEF"), Solution::U32(38)],
            input: "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
                
",
        },
    ];
}
