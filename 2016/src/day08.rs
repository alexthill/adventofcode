use aoc_lib_rust::{next, next_parse, Day, Solution};

pub struct Day08;

impl Day for Day08 {

    const PART1: Solution = Solution::U32(128);
    const PART2: Solution = Solution::Str("
####  ##   ##  ###   ##  ###  #  # #   # ##   ##  
#    #  # #  # #  # #  # #  # #  # #   ##  # #  # 
###  #  # #  # #  # #    #  # ####  # # #  # #  # 
#    #  # #### ###  # ## ###  #  #   #  #### #  # 
#    #  # #  # # #  #  # #    #  #   #  #  # #  # 
####  ##  #  # #  #  ### #    #  #   #  #  #  ##  ");

    fn solve(input: &str) -> [Solution; 2] {
        let mut display = [0_u64; 6];
        for line in input.lines() {
            let mut parts = line.split(' ');
            match (next!(parts), next!(parts)) {
                ("rect", dims) => {
                    let mut parts = dims.split('x');
                    let x = next_parse!(parts, u64);
                    let y = next_parse!(parts, usize);
                    for row in display.iter_mut().take(y) {
                        *row |= !(!0 << x);
                    }
                }
                ("rotate", "column") => {
                    let x = next_parse!(next!(parts).split('=').skip(1), u32);
                    assert_eq!(parts.next(), Some("by"));
                    let by = next_parse!(parts, i32);
                    let copy = display;
                    for y in 0..6 {
                        let on = (copy[(y as i32 - by).rem_euclid(6) as usize] & (1 << x)) != 0;
                        if on {
                            display[y] |= 1 << x;
                        } else {
                            display[y] &= !(1 << x);
                        }
                    }
                }
                ("rotate", "row") => {
                    let y = next_parse!(next!(parts).split('=').skip(1), usize);
                    assert_eq!(parts.next(), Some("by"));
                    let by = next_parse!(parts, u32);
                    display[y] = (display[y] << by) & ((1 << 50) - 1) | (display[y] >> (50 - by));
                }
                _ => unreachable!(),
            }
        }
        let sol1 = display.iter().map(|row| row.count_ones()).sum();
        let mut sol2 = String::new();
        for row in display {
            sol2.push('\n');
            for y in 0..50 {
                let bit = (row >> y) & 1;
                sol2.push(if bit == 1 { '#' } else { ' ' });
            }
        }
        [Solution::U32(sol1), Solution::String(sol2)]
    }
}
