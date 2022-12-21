use crate::Solution;
use std::collections::HashMap;

pub const SOLUTION: Solution = Solution::U64((54703080378102, 3952673930912));

pub fn solve(input: String) -> Solution {
    let input = input.into_bytes();
    let mut shouting = Vec::new();
    let mut waiting = HashMap::new();
    let mut monkeys = Vec::new();
    let mut i = 0;
    let mut humn_num = -1;
    while i < input.len() {
        let name = [input[i], input[i + 1], input[i + 2], input[i + 3]];
        if input[i + 6].is_ascii_digit() {
            let num = if input[i + 7].is_ascii_digit() {
                if input[i + 8].is_ascii_digit() {
                    i += 9;
                    (input[i - 3] - b'0') as i64 * 100 + ((input[i - 2] - b'0') * 10 + (input[i - 1] - b'0')) as i64
                } else {
                    i += 8;
                    ((input[i - 2] - b'0') * 10 + (input[i - 1] - b'0')) as i64
                }
            } else {
                i += 7;
                (input[i - 1] - b'0') as i64
            };
            if name != *b"humn" {
                shouting.push((name, num));
            } else {
                humn_num = num;
            }
            debug_assert_eq!(input[i], b'\n');
            i += 1;
        } else {
            let left = &input[i + 6..i + 10];
            let op = input[i + 11];
            let right = &input[i + 13..i + 17];
            assert!(waiting.insert(left, (monkeys.len(), 0)).is_none());
            assert!(waiting.insert(right, (monkeys.len(), 1)).is_none());
            monkeys.push((name, op, [-1, -1]));
            debug_assert_eq!(input[i + 17], b'\n');
            i += 18;
        }
    }
    debug_assert_ne!(humn_num, -1, "humn not found");
    
    let mut waiting_count = waiting.len();
    while let Some((name, num)) = shouting.pop() {
        let &(idx, n) = waiting.get(name.as_ref()).unwrap();
        let monkey = monkeys.get_mut(idx).unwrap();
        monkey.2[n] = num;
        if monkey.2[n ^ 1] != -1 {
            let new_num = match monkey.1 {
                b'+' => monkey.2[0] + monkey.2[1],
                b'-' => monkey.2[0] - monkey.2[1],
                b'*' => monkey.2[0] * monkey.2[1],
                b'/' => monkey.2[0] / monkey.2[1],
                other => panic!("unknown op {}", std::char::from_u32(other as u32).unwrap()),
            };
            shouting.push((monkey.0, new_num));
            waiting_count -= 1;
        }
    }
    
    let mut name = *b"humn";
    let mut num = humn_num;
    let mut seq = Vec::with_capacity(waiting_count);
    while let Some(&(idx, n)) = waiting.get(name.as_ref()) {
        let monkey = monkeys.get_mut(idx).unwrap();
        let other_num = monkey.2[n ^ 1];
        debug_assert_ne!(other_num, -1);
        name = monkey.0;
        num = match monkey.1 {
            b'+' => {
                seq.push((other_num, b'-'));
                num + other_num
            }
            b'-' => {
                if n == 0 {
                    seq.push((other_num, b'+'));
                    num - other_num
                } else {
                    seq.push((-1, b'*'));
                    seq.push((other_num, b'-'));
                    other_num - num
                }
            }
            b'*' => {
                seq.push((other_num, b'/'));
                num * other_num
            }
            b'/' => {
                if n == 0 {
                    seq.push((other_num, b'*'));
                    num / other_num
                } else {
                    panic!("unexpected num / seq")
                }
            }
            other => unknow_op(other),
        };
    }
    debug_assert_eq!(name, *b"root");
    let res1 = num;
    
    let mut res2 = seq.pop().unwrap().0;
    for &(num, op) in seq.iter().rev() {
        res2 = match op {
            b'+' => res2 + num,
            b'-' => res2 - num,
            b'*' => res2 * num,
            b'/' => res2 / num,
            other => unknow_op(other),
        };
    }
    
    (res1 as u64, res2 as u64).into()
}

fn unknow_op(op: u8) -> ! {
    panic!("unknown op {}", std::char::from_u32(op as u32).unwrap());
}