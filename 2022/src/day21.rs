use crate::Solution;
use std::collections::HashMap;

pub const SOLUTION: Solution = Solution::U64((54703080378102, 3952673930912));

pub fn solve(input: String) -> Solution {
    let mut shouting = Vec::new();
    let mut monkeys = Vec::new();
    let mut waiting = HashMap::new();
    let mut humn_num = -1;
    for line in input.as_bytes().split(|c| *c == b'\n') {
        let name = &line[..4];
        if line[6].is_ascii_digit() {
            let num = line[6..].iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as i64);
            shouting.push((name, num));
            if name == b"humn" {
                humn_num = num;
            }
        } else {
            let (left, op, right) = (&line[6..10], line[11], &line[13..17]);
            assert!(waiting.insert(left, (monkeys.len(), 0)).is_none());
            assert!(waiting.insert(right, (monkeys.len(), 1)).is_none());
            monkeys.push((name, op, [-1, -1]));
        }
    }
    debug_assert_ne!(humn_num, -1, "humn not found");

    let execute_op = |op, a, b| match op {
        b'+' => a + b,
        b'-' => a - b,
        b'!' => b - a,
        b'*' => a * b,
        b'/' => a / b,
        _ => unreachable!(),
    };

    while let Some((name, num)) = shouting.pop() {
        let &(idx, n) = waiting.get(name.as_ref()).unwrap();
        let monkey = monkeys.get_mut(idx).unwrap();
        monkey.2[n] = num;
        if monkey.2[n ^ 1] != -1 {
            shouting.push((monkey.0, execute_op(monkey.1, monkey.2[0], monkey.2[1])));
        }
    }

    let mut name = &b"humn"[..];
    let mut sol1 = humn_num;
    let mut seq = Vec::new();
    while let Some(&(idx, n)) = waiting.get(name) {
        let monkey = monkeys.get_mut(idx).unwrap();
        let other_num = monkey.2[n ^ 1];
        debug_assert_ne!(other_num, -1);
        name = monkey.0;
        let (op, new_num) = match monkey.1 {
            b'+'           => (b'-', sol1 + other_num),
            b'-' if n == 0 => (b'+', sol1 - other_num),
            b'-'           => (b'!', other_num - sol1),
            b'*'           => (b'/', sol1 * other_num),
            b'/' if n == 0 => (b'*', sol1 / other_num),
            _ => unreachable!(),
        };
        sol1 = new_num;
        seq.push((other_num, op));
    }
    debug_assert_eq!(name, b"root");

    let start_val = seq.pop().unwrap().0;
    let sol2 = seq.into_iter().rev().fold(start_val, |acc, (num, op)| execute_op(op, acc, num));

    (sol1 as u64, sol2 as u64).into()
}
