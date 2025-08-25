use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;

enum Val {
    Reg(usize),
    Value(i64),
}

enum Op {
    Snd(Val),
    Set(usize, Val),
    Add(usize, Val),
    Mul(usize, Val),
    Mod(usize, Val),
    Rcv(usize),
    Jgz(Val, Val),
}

pub struct Day18;

impl Day for Day18 {

    const PART1: Solution = Solution::U32(8600);
    const PART2: Solution = Solution::U32(7239);

    fn solve(input: &str) -> [Solution; 2] {
        fn get_operand(operand: &str) -> Val {
            if operand.len() == 1 && operand.as_bytes()[0].is_ascii_alphabetic() {
                Val::Reg((operand.as_bytes()[0] - b'a') as usize)
            } else {
                Val::Value(operand.parse().unwrap())
            }
        }

        let ops = input.lines().map(|line| {
            let mut parts = line.split(' ');
            let op = parts.next().unwrap();
            let a = parts.next().unwrap();
            let b = parts.next();

            match op {
                "snd" => Op::Snd(get_operand(a)),
                "set" => Op::Set((a.as_bytes()[0] - b'a') as usize, get_operand(b.unwrap())),
                "add" => Op::Add((a.as_bytes()[0] - b'a') as usize, get_operand(b.unwrap())),
                "mul" => Op::Mul((a.as_bytes()[0] - b'a') as usize, get_operand(b.unwrap())),
                "mod" => Op::Mod((a.as_bytes()[0] - b'a') as usize, get_operand(b.unwrap())),
                "rcv" => Op::Rcv((a.as_bytes()[0] - b'a') as usize),
                "jgz" => Op::Jgz(get_operand(a), get_operand(b.unwrap())),
                other => panic!("unknow instruction {other}"),
            }
        }).collect::<Vec<_>>();

        let mut send = 0;
        let mut regs = [0; 26];
        let mut ip = 0;
        let sol1 = loop {
            match ops[ip as usize] {
                Op::Snd(Val::Reg(idx)) => send = regs[idx],
                Op::Snd(Val::Value(val)) => send = val,
                Op::Set(reg, Val::Reg(idx)) => regs[reg] = regs[idx],
                Op::Set(reg, Val::Value(val)) => regs[reg] = val,
                Op::Add(reg, Val::Reg(idx)) => regs[reg] += regs[idx],
                Op::Add(reg, Val::Value(val)) => regs[reg] += val,
                Op::Mul(reg, Val::Reg(idx)) => regs[reg] *= regs[idx],
                Op::Mul(reg, Val::Value(val)) => regs[reg] *= val,
                Op::Mod(reg, Val::Reg(idx)) => regs[reg] %= regs[idx],
                Op::Mod(reg, Val::Value(val)) => regs[reg] %= val,
                Op::Rcv(idx) => if regs[idx] != 0 { break send; },
                Op::Jgz(Val::Reg(x), Val::Reg(y)) if regs[x] > 0 => ip += regs[y] - 1,
                Op::Jgz(Val::Reg(x), Val::Value(y)) if regs[x] > 0 => ip += y - 1,
                Op::Jgz(Val::Value(x), Val::Reg(y)) if x > 0 => ip += regs[y] - 1,
                Op::Jgz(Val::Value(x), Val::Value(y)) if x > 0 => ip += y - 1,
                Op::Jgz(_, _) => {}
            }
            ip += 1;
        };

        fn execute_op(
            op: &Op,
            ip: &mut i64,
            regs: &mut [i64],
            queue_self: &mut VecDeque<i64>,
            queue_other: &mut VecDeque<i64>,
        ) -> bool {
            match *op {
                Op::Snd(Val::Reg(idx)) => queue_other.push_back(regs[idx]),
                Op::Snd(Val::Value(val)) => queue_other.push_back(val),
                Op::Set(reg, Val::Reg(idx)) => regs[reg] = regs[idx],
                Op::Set(reg, Val::Value(val)) => regs[reg] = val,
                Op::Add(reg, Val::Reg(idx)) => regs[reg] += regs[idx],
                Op::Add(reg, Val::Value(val)) => regs[reg] += val,
                Op::Mul(reg, Val::Reg(idx)) => regs[reg] *= regs[idx],
                Op::Mul(reg, Val::Value(val)) => regs[reg] *= val,
                Op::Mod(reg, Val::Reg(idx)) => regs[reg] %= regs[idx],
                Op::Mod(reg, Val::Value(val)) => regs[reg] %= val,
                Op::Rcv(idx) => {
                    let Some(val) = queue_self.pop_front() else { return true };
                    regs[idx] = val;
                }
                Op::Jgz(Val::Reg(x), Val::Reg(y)) if regs[x] > 0 => *ip += regs[y] - 1,
                Op::Jgz(Val::Reg(x), Val::Value(y)) if regs[x] > 0 => *ip += y - 1,
                Op::Jgz(Val::Value(x), Val::Reg(y)) if x > 0 => *ip += regs[y] - 1,
                Op::Jgz(Val::Value(x), Val::Value(y)) if x > 0 => *ip += y - 1,
                Op::Jgz(_, _) => {}
            }
            *ip += 1;
            return false;
        }
        let mut queue0 = VecDeque::new();
        let mut queue1 = VecDeque::new();
        let mut regs = [[0; 26]; 2];
        let mut ip = [0; 2];
        let mut sol2 = 0;
        regs[0][(b'p' - b'a') as usize] = 0;
        regs[1][(b'p' - b'a') as usize] = 1;
        loop {
            let Some(op) = ip[0].try_into().ok().and_then(|ip: usize| ops.get(ip)) else { break };
            let wait0 = execute_op(op, &mut ip[0], &mut regs[0], &mut queue0, &mut queue1);

            let queue0_len = queue0.len();
            let Some(op) = ip[1].try_into().ok().and_then(|ip: usize| ops.get(ip)) else { break };
            let wait1 = execute_op(op, &mut ip[1], &mut regs[1], &mut queue1, &mut queue0);
            sol2 += (queue0.len() != queue0_len) as u32;

            if wait0 && wait1 {
                break;
            }
        }

        [Solution::U32(sol1 as _), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(4), Solution::None],
            input: "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2",
        },
    ];
}
