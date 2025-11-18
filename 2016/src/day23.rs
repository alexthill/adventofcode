use aoc_lib_rust::{next, Day, Example, Solution};

#[derive(Debug, Clone, Copy)]
enum Instruction {
    CpyReg(u8, u8),
    CpyVal(i8, u8),
    Inc(u8),
    Dec(u8),
    JnzRegVal(u8, i8),
    JnzValVal(i8, i8),
    JnzRegReg(u8, u8),
    JnzValReg(i8, u8),
    Toggle(u8),
    Nop,
}

impl Instruction {
    fn toggle(&self) -> Self {
        match *self {
            Self::Inc(x) => Self::Dec(x),
            Self::Dec(x) => Self::Inc(x),
            Self::Toggle(x) => Self::Inc(x),
            Self::JnzRegReg(a, b) => Self::CpyReg(a, b),
            Self::JnzValReg(a, b) => Self::CpyVal(a, b),
            Self::CpyReg(a, b) => Self::JnzRegReg(a, b),
            Self::CpyVal(a, b) => Self::JnzValReg(a, b),
            _ => Self::Nop,
        }
    }
}

pub struct Day23;

impl Day for Day23 {

    const PART1: Solution = Solution::U32(11514);
    const PART2: Solution = Solution::U32(479008074);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let prog = input.lines().map(|line| {
            let mut parts = line.as_bytes().split(|ch| *ch == b' ');
            let instr = match next!(parts) {
                b"cpy" => {
                    let from = next!(parts);
                    let to = next!(parts)[0] - b'a';
                    if from[0].is_ascii_alphabetic() {
                        Instruction::CpyReg(from[0] - b'a', to)
                    } else {
                        let val = str::from_utf8(from).unwrap().parse().unwrap();
                        Instruction::CpyVal(val, to)
                    }
                }
                b"inc" => Instruction::Inc(next!(parts)[0] - b'a'),
                b"dec" => Instruction::Dec(next!(parts)[0] - b'a'),
                b"jnz" => {
                    let val = next!(parts);
                    let offset = next!(parts);
                    if val[0].is_ascii_alphabetic() {
                        if offset[0].is_ascii_alphabetic() {
                            Instruction::JnzRegReg(val[0] - b'a', offset[0] - b'a')
                        } else {
                            let offset = str::from_utf8(offset).unwrap().parse().unwrap();
                            Instruction::JnzRegVal(val[0] - b'a', offset)
                        }
                    } else {
                        let val = str::from_utf8(val).unwrap().parse().unwrap();
                        if offset[0].is_ascii_alphabetic() {
                            Instruction::JnzValReg(val, offset[0] - b'a')
                        } else {
                            let offset = str::from_utf8(offset).unwrap().parse().unwrap();
                            Instruction::JnzValVal(val, offset)
                        }
                    }
                }
                b"tgl" => Instruction::Toggle(next!(parts)[0] - b'a'),
                _ => unreachable!(),
            };
            (instr, false)
        }).collect::<Vec<_>>();

        fn run(prog: &mut [(Instruction, bool)], regs: &mut [i32]) {
            let mut ip = 0;
            while let Some((instr, _)) = prog.get(ip as usize) {
                match *instr {
                    Instruction::CpyReg(from, to) => regs[to as usize] = regs[from as usize],
                    Instruction::CpyVal(val, to) => regs[to as usize] = val as _,
                    Instruction::Inc(reg) => regs[reg as usize] += 1,
                    Instruction::Dec(reg) => regs[reg as usize] -= 1,
                    Instruction::JnzRegVal(reg, offset) if regs[reg as usize] != 0 =>
                        ip += offset - 1,
                    Instruction::JnzValVal(val, offset) if val != 0 =>
                        ip += offset - 1,
                    Instruction::JnzRegReg(reg, offset) if regs[reg as usize] != 0 => {
                        let Ok(offset): Result<i8, _> = regs[offset as usize].try_into() else {
                            break
                        };
                        ip += offset - 1;
                    }
                    Instruction::JnzValReg(val, offset) if val != 0 => {
                        let Ok(offset): Result<i8, _> = regs[offset as usize].try_into() else {
                            break
                        };
                        ip += offset - 1;
                    }
                    Instruction::Toggle(reg) => {
                        let offset = regs[reg as usize];
                        match prog.get_mut((ip + offset as i8) as usize) {
                            Some(instr @ (_, false)) => {
                                instr.0 = instr.0.toggle();
                                instr.1 = true;
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                ip += 1;
            }
        }

        fn factorial(n: u32) -> u32 {
            (2..=n).product()
        }

        let mut regs = [7, 0, 0, 0];
        let mut prog_cpy = prog.clone();
        run(&mut prog_cpy, &mut regs);
        let sol1 = regs[0] as u32;

        let sol2 = if example {
            0
        } else {
            let offset = sol1 - factorial(7);
            factorial(12) + offset
        };

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::U32(0)],
            input: "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a",
        },
    ];
}
