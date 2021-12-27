#![allow(dead_code)]
use std::fs;
use std::collections::HashMap;

enum Instruction {
    Inp(usize),
    Add(usize, usize),
    AddNum(usize, i64),
    Mul(usize, usize),
    MulNum(usize, i64),
    Div(usize, usize),
    DivNum(usize, i64),
    Mod(usize, usize),
    ModNum(usize, i64),
    Eql(usize, usize),
    EqlNum(usize, i64),
    Set(usize, usize),
    SetNum(usize, i64),
    SetZero(usize),
}
impl Instruction {
    fn register_name_to_num(name: &str) -> Option<usize> {
        match name {
            "x" => Some(0),
            "y" => Some(1),
            "z" => Some(2),
            "w" => Some(3),
            _ => None,
        }
    }
    
    fn create_prog(string: &str) -> Box::<[Self]> {
        let prog = string.lines().filter_map(|line| {
            match Self::new(line) {
                Self::DivNum(_, 1) => None,
                Self::MulNum(reg, 0) => Some(Self::SetZero(reg)),
                other => Some(other),
            }
        }).collect::<Vec<Self>>();
        let mut mod_prog = vec![];
        let mut prog_iter = prog.into_iter();
        while let Some(instr) = prog_iter.next() {
            match instr {
                Self::SetZero(reg) => {
                    match prog_iter.next() {
                        Some(Self::Add(op1, op2)) if reg == op1 => mod_prog.push(Self::Set(op1, op2)),
                        Some(Self::AddNum(op1, op2)) if reg == op1 => mod_prog.push(Self::SetNum(op1, op2)),
                        Some(other) => {
                            mod_prog.push(Self::SetZero(reg));
                            mod_prog.push(other);
                        }
                        None => mod_prog.push(Self::SetZero(reg)),
                    }
                },
                other => mod_prog.push(other),
            }
        }
        mod_prog.into_boxed_slice()
    }
    
    fn new(string: &str) -> Self {
        let mut parts = string.split(' ');
        let instr = parts.next().unwrap();
        let op1 = parts.next().unwrap();
        let op2 = parts.next();
        match instr {
            "inp" => Self::Inp(Self::register_name_to_num(op1).unwrap()),
            "add" => {
                let op2 = op2.unwrap();
                if let Some(num) = Self::register_name_to_num(op2) {
                    Self::Add(Self::register_name_to_num(op1).unwrap(), num)
                } else {
                    Self::AddNum(Self::register_name_to_num(op1).unwrap(), op2.parse::<i64>().unwrap())
                }
            }
            "mul" => {
                let op2 = op2.unwrap();
                if let Some(num) = Self::register_name_to_num(op2) {
                    Self::Mul(Self::register_name_to_num(op1).unwrap(), num)
                } else {
                    Self::MulNum(Self::register_name_to_num(op1).unwrap(), op2.parse::<i64>().unwrap())
                }
            }
            "div" => {
                let op2 = op2.unwrap();
                if let Some(num) = Self::register_name_to_num(op2) {
                    Self::Div(Self::register_name_to_num(op1).unwrap(), num)
                } else {
                    Self::DivNum(Self::register_name_to_num(op1).unwrap(), op2.parse::<i64>().unwrap())
                }
            }
            "mod" => {
                let op2 = op2.unwrap();
                if let Some(num) = Self::register_name_to_num(op2) {
                    Self::Mod(Self::register_name_to_num(op1).unwrap(), num)
                } else {
                    Self::ModNum(Self::register_name_to_num(op1).unwrap(), op2.parse::<i64>().unwrap())
                }
            }
            "eql" => {
                let op2 = op2.unwrap();
                if let Some(num) = Self::register_name_to_num(op2) {
                    Self::Eql(Self::register_name_to_num(op1).unwrap(), num)
                } else {
                    Self::EqlNum(Self::register_name_to_num(op1).unwrap(), op2.parse::<i64>().unwrap())
                }
            }
            other => panic!("unknown instruction {}", other),
        }
    }
    
    fn exec_prog(prog: &[Self], input: &[u8]) -> [i64; 4] {
        let mut registers = [0; 4];
        let mut input_pos = 0;
        for instr in prog.iter() {
            instr.exec(&mut registers, input, &mut input_pos);
        }
        registers
    }
    
    fn exec(&self, registers: &mut [i64; 4], input: &[u8], input_pos: &mut usize) {
        match *self {
            Self::Inp(op1) => {
                registers[op1] = input[*input_pos] as i64;
                *input_pos += 1;
            }
            Self::Add(op1, op2) => registers[op1] = registers[op1] + registers[op2],
            Self::AddNum(op1, op2) => registers[op1] = registers[op1] + op2,
            Self::Mul(op1, op2) => registers[op1] = registers[op1] * registers[op2],
            Self::MulNum(op1, op2) => registers[op1] = registers[op1] * op2,
            Self::Div(op1, op2) => registers[op1] = registers[op1] / registers[op2],
            Self::DivNum(op1, op2) => registers[op1] = registers[op1] / op2,
            Self::Mod(op1, op2) => registers[op1] = registers[op1] % registers[op2],
            Self::ModNum(op1, op2) => registers[op1] = registers[op1] % op2,
            Self::Eql(op1, op2) => registers[op1] = if registers[op1] == registers[op2] { 1 } else { 0 },
            Self::EqlNum(op1, op2) => registers[op1] = if registers[op1] == op2 { 1 } else { 0 },
            Self::Set(op1, op2) => registers[op1] = registers[op2],
            Self::SetNum(op1, op2) => registers[op1] = op2,
            Self::SetZero(op1) => registers[op1] = 0,
        }
    }
    
    fn exec2(&self, registers: &mut [i64; 4]) {
        match *self {
            Self::Inp(_) => panic!("cannot execute inp"),
            Self::Add(op1, op2) => registers[op1] = registers[op1] + registers[op2],
            Self::AddNum(op1, op2) => registers[op1] = registers[op1] + op2,
            Self::Mul(op1, op2) => registers[op1] = registers[op1] * registers[op2],
            Self::MulNum(op1, op2) => registers[op1] = registers[op1] * op2,
            Self::Div(op1, op2) => registers[op1] = registers[op1] / registers[op2],
            Self::DivNum(op1, op2) => registers[op1] = registers[op1] / op2,
            Self::Mod(op1, op2) => registers[op1] = registers[op1] % registers[op2],
            Self::ModNum(op1, op2) => registers[op1] = registers[op1] % op2,
            Self::Eql(op1, op2) => registers[op1] = if registers[op1] == registers[op2] { 1 } else { 0 },
            Self::EqlNum(op1, op2) => registers[op1] = if registers[op1] == op2 { 1 } else { 0 },
            Self::Set(op1, op2) => registers[op1] = registers[op2],
            Self::SetNum(op1, op2) => registers[op1] = op2,
            Self::SetZero(op1) => registers[op1] = 0,
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/24.txt").unwrap();
    let prog = Instruction::create_prog(&input);
    let mut states = HashMap::new();
    let mut input_num = 0;
    states.insert([0; 4], 0);
    for instr in prog.into_iter() {
        let mut new_states = HashMap::new();
        if let Instruction::Inp(reg) = instr {
            for (state, input) in states.drain() {
                for i in if input_num == 0 { 6..=6 } else { 1..=9 } {
                    let input = input * 10 + i;
                    let mut state = state.clone();
                    state[*reg] = i;
                    let best_input = new_states.entry(state).or_insert(input);
                    if input > *best_input {
                        *best_input = input;
                    }
                }
            }
            input_num += 1;
            println!("state count after input {}: {}", input_num, new_states.len());
        } else {
            for (mut state, input) in states.drain() {
                instr.exec2(&mut state);
                let best_input = new_states.entry(state).or_insert(input);
                if input > *best_input {
                    *best_input = input;
                }
            }
        }
        states = new_states;
    }
    let best = states.drain().filter(|(state, _)| state[2] == 0).map(|(_, input)| input).min();
    println!("best input: {:?}", best);
    
    println!("The 1. solution should be {}", 65984919991933_i64);
    println!("The 2. solution should be {}", 11211619541713_i64);
    println!("But for some reason my code does not work correctly");
}

fn num_to_digits(mut n: u64) -> [u8; 14] {
    let mut digits = [0; 14];
    for i in 1..=14 {
        digits[14 - i] = (n % 10) as u8;
        n = n / 10;
    }
    digits
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test() {
        let prog = Instruction::create_prog(r#"inp z
inp x
mul z 3
eql z x"#);
        let res = Instruction::exec_prog(&prog, &[1, 3]);
        assert_eq!(res[2], 1);
        let res = Instruction::exec_prog(&prog, &[1, 7]);
        assert_eq!(res[2], 0);
    }
    
    #[test]
    fn test_add() {
        let prog = Instruction::create_prog(r#"inp x
inp y
add x y"#);
        let res = Instruction::exec_prog(&prog, &[1, 3]);
        assert_eq!(res[0], 4);
    }
    
    #[test]
    fn test_div() {
        let prog = Instruction::create_prog(r#"inp x
inp y
div x y"#);
        let res = Instruction::exec_prog(&prog, &[9, 2]);
        assert_eq!(res[0], 4);
    }
    
    #[test]
    fn test_mod() {
        let prog = Instruction::create_prog(r#"inp x
inp y
mod x y"#);
        let res = Instruction::exec_prog(&prog, &[9, 5]);
        assert_eq!(res[0], 4);
    }
    
    #[test]
    fn test_optimize() {
        let prog = Instruction::create_prog(r#"inp x
inp y
mul x 0
add x y"#);
        let res = Instruction::exec_prog(&prog, &[9, 5]);
        assert_eq!(prog.len(), 3);
        assert_eq!(res[0], 5);
        
        let prog = Instruction::create_prog(r#"inp x
inp y
mul x 0
add x 42"#);
        let res = Instruction::exec_prog(&prog, &[9, 5]);
        assert_eq!(prog.len(), 3);
        assert_eq!(res[0], 42);
    }
}
