use aoc_lib_rust::{Day, Example, Solution};
use std::cmp::Reverse;

pub struct Day04;

impl Day for Day04 {

    const PART1: Solution = Solution::U32(278221);
    const PART2: Solution = Solution::U32(267);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol2 = 0;
        let sol1 = input.lines().filter_map(|line| {
            let mut counts = [0; 26];
            let mut iter = line.bytes();
            let mut id = 0;
            for ch in &mut iter {
                if ch == b'-' {
                    continue;
                }
                if ch.is_ascii_alphabetic() {
                    counts[(ch - b'a') as usize] += 1
                } else if ch.is_ascii_digit() {
                    id = (ch - b'0') as u32;
                    break;
                } else {
                    unreachable!();
                }
            }
            for ch in &mut iter {
                if ch.is_ascii_digit() {
                    id = id * 10 + (ch - b'0') as u32;
                } else {
                    assert_eq!(ch, b'[');
                    break;
                }
            }
            let mut counts = counts.into_iter().enumerate().collect::<Vec<_>>();
            counts.sort_by_key(|x| Reverse(x.1));
            for ((i, _), ch) in counts.iter().zip(&mut iter) {
                if ch == b']' {
                    break;
                }
                if *i as u8 + b'a' != ch {
                    return None;
                }
            }
            let name = line.bytes()
                .take_while(|ch| !ch.is_ascii_digit())
                .map(|ch| {
                    if ch == b'-' {
                        ' '
                    } else {
                        char::from_u32((((ch - b'a') as u32 + id) % 26) + b'a' as u32).unwrap()
                    }
                }).collect::<String>();
            if name == "northpole object storage " {
                sol2 = id;
            }
            Some(id)
        }).sum();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1514), Solution::None],
            input: "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]",
        },
    ];
}
