use aoc_lib_rust::{Day, Solution};
use crate::day12::Instruction;

pub struct Day25;

impl Day for Day25 {

    const PART1: Solution = Solution::U32(196);
    const PART2: Solution = Solution::None;

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Instruction::parse_prog(input);

        fn run(prog: &[Instruction], regs: &mut [u32]) -> [u32; 12] {
            let mut ip = 0;
            let mut clock_count = 0;
            let mut clocks = [0; 12];
            while let Some(instr) = prog.get(ip as usize) {
                match *instr {
                    Instruction::CpyReg(from, to) => regs[to as usize] = regs[from as usize],
                    Instruction::CpyVal(val, to) => regs[to as usize] = val as _,
                    Instruction::Inc(reg) => regs[reg as usize] += 1,
                    Instruction::Dec(reg) => regs[reg as usize] -= 1,
                    Instruction::JnzReg(reg, offset) if regs[reg as usize] != 0 => ip += offset - 1,
                    Instruction::JnzVal(val, offset) if val != 0 => ip += offset - 1,
                    Instruction::Out(reg) => {
                        clocks[clock_count] = regs[reg as usize];
                        clock_count += 1;
                        if clock_count == clocks.len() {
                            return clocks;
                        }
                    }
                    _ => {}
                }
                ip += 1;
            }
            unreachable!()
        }

        let mut offset = 0;
        let mut period = 1;
        'outer: for i in 1..=12 {
            for a in (offset..).step_by(period) {
                let mut regs = [a, 0, 0, 0];
                let clocks = run(&prog, &mut regs);
                if clocks.iter().take(i).enumerate().all(|(i, clock)| *clock == i as u32 % 2) {
                    offset = a;
                    if clocks.iter().skip(i).enumerate().all(|(i, clock)| *clock == i as u32 % 2) {
                        break 'outer;
                    }
                    break;
                }
            }
            period *= 2;
        }
        let sol1 = offset;

        [Solution::U32(sol1), Solution::None]
    }
}
