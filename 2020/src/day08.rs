use aoc_lib_rust::{Day, Example, Solution};

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Acc,
    Jmp,
    Nop,
}

pub struct Day08;

impl Day for Day08 {

    const PART1: Solution = Solution::U32(1563);
    const PART2: Solution = Solution::U32(767);

    fn solve(input: &str) -> [Solution; 2] {
        let mut ops = Vec::new();
        for line in input.as_bytes().split(|c| *c == b'\n') {
            let mut parts = line.split(|c| *c == b' ');
            let op = match parts.next().unwrap() {
                b"acc" => Op::Acc,
                b"jmp" => Op::Jmp,
                b"nop" => Op::Nop,
                _ => unreachable!(),
            };
            let num = parts.next().unwrap();
            let sign = (num[0] == b'+') as i32 * 2 - 1;
            let num = sign * num[1..].iter().fold(0, |acc, c| acc * 10 + (*c - b'0') as i32);
            ops.push((op, num, true));
        }

        fn run_prog(ops: &mut [(Op, i32, bool)]) -> Result<u32, u32> {
            let mut pc = 0;
            let mut acc = 0;
            loop {
                let op = match ops.get_mut(pc as usize) {
                    Some((_, _, false)) => return Err(acc as u32),
                    Some(op) => op,
                    None => return Ok(acc as u32),
                };
                op.2 = false;
                pc += 1;
                match op.0 {
                    Op::Acc => acc += op.1,
                    Op::Jmp => pc += op.1 - 1,
                    _ => {}
                }
            }
        }

        let sol1 = run_prog(&mut ops).unwrap_err();
        let mut i = 0;
        let sol2 = loop {
            let op = &mut ops[i];
            let old_op = op.0;
            i += 1;
            match op.0 {
                Op::Acc => continue,
                Op::Jmp => op.0 = Op::Nop,
                Op::Nop => op.0 = Op::Jmp,
            }
            for op in ops.iter_mut() {
                op.2 = true;
            }
            if let Ok(acc) = run_prog(&mut ops) {
                break acc;
            }
            ops[i - 1].0 = old_op;
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(5), Solution::U32(8)],
            input: "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6",
        },
    ];
}
