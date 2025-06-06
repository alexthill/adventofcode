use aoc_lib_rust::{Day, Solution};
use super::day19::{exec_op, parse_instructions};

pub struct Day21;

impl Day for Day21 {

    const PART1: Solution = Solution::U32(15690445);
    const PART2: Solution = Solution::U32(936387);

    fn solve(input: &str) -> [Solution; 2] {
        let (ip, instructions) = parse_instructions(input);

        let mut registers = [0; 6];
        let sol1 = loop {
            let Some((code, args)) = instructions.get(registers[ip] as usize) else {
                panic!("ran out of instructions before finding solution");
            };
            if *code == 15 { // eqrr
                break registers[args[0] as usize];
            }
            registers = exec_op(*code as u8, *args, registers);
            registers[ip] += 1;
        };

        let mut cache = vec![false; 16777215];
        let mut last_value = 0;
        let sol2 = loop {
            let Some((code, args)) = instructions.get(registers[ip] as usize) else {
                panic!("ran out of instructions before finding solution");
            };
            if *code == 15 { // eqrr
                let value = registers[args[0] as usize];
                if cache[value as usize] {
                    break last_value;
                }
                cache[value as usize] = true;
                last_value = value;
            }
            registers = exec_op(*code as u8, *args, registers);
            registers[ip] += 1;
        };

        [Solution::U64(sol1), Solution::U64(sol2)]
    }
}
