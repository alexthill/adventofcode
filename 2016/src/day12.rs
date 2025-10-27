use aoc_lib_rust::{next, Day, Example, Solution};

#[derive(Debug, Clone, Copy)]
enum Instruction {
    CpyReg(u8, u8),
    CpyVal(u8, u8),
    Inc(u8),
    Dec(u8),
    JnzReg(u8, i8),
    JnzVal(u8, i8),
}

pub struct Day12;

impl Day for Day12 {

    const PART1: Solution = Solution::U32(318007);
    const PART2: Solution = Solution::U32(9227661);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = input.lines().map(|line| {
            let mut parts = line.as_bytes().split(|ch| *ch == b' ');
            match next!(parts) {
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
                    let offset = str::from_utf8(next!(parts)).unwrap().parse().unwrap();
                    if val[0].is_ascii_alphabetic() {
                        Instruction::JnzReg(val[0] - b'a', offset)
                    } else {
                        let val = str::from_utf8(val).unwrap().parse().unwrap();
                        Instruction::JnzVal(val, offset)
                    }
                }
                _ => unreachable!(),
            }
        }).collect::<Vec<_>>();

        fn run(prog: &[Instruction], regs: &mut [u32]) {
            let mut ip = 0;
            while let Some(instr) = prog.get(ip as usize) {
                match *instr {
                    Instruction::CpyReg(from, to) => regs[to as usize] = regs[from as usize],
                    Instruction::CpyVal(val, to) => regs[to as usize] = val as _,
                    Instruction::Inc(reg) => regs[reg as usize] += 1,
                    Instruction::Dec(reg) => regs[reg as usize] -= 1,
                    Instruction::JnzReg(reg, offset) if regs[reg as usize] != 0 => ip += offset - 1,
                    Instruction::JnzVal(val, offset) if val != 0 => ip += offset - 1,
                    _ => {}
                }
                ip += 1;
            }
        }

        let mut regs = [0; 4];
        run(&prog, &mut regs);
        let sol1 = regs[0];

        let mut regs = [0, 0, 1, 0];
        run(&prog, &mut regs);
        let sol2 = regs[0];

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(42), Solution::None],
            input: "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a",
        },
    ];
}
