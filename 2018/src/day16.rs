use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::next_parse;

pub struct Day16;

impl Day for Day16 {

    const PART1: Solution = Solution::U32(542);
    const PART2: Solution = Solution::U32(575);

    fn solve(input: &str) -> [Solution; 2] {
        fn exec_op(code: u8, args: [u32; 3], mut registers: [u32; 4]) -> [u32; 4] {
            let [a, b, c] = args.map(|arg| arg as usize);
            match code {
                0 => registers[c] = registers[a] + registers[b],
                1 => registers[c] = registers[a] + args[1],
                2 => registers[c] = registers[a] * registers[b],
                3 => registers[c] = registers[a] * args[1],
                4 => registers[c] = registers[a] & registers[b],
                5 => registers[c] = registers[a] & args[1],
                6 => registers[c] = registers[a] | registers[b],
                7 => registers[c] = registers[a] | args[1],
                8 => registers[c] = registers[a],
                9 => registers[c] = args[0],
                10 => registers[c] = (args[0] > registers[b]) as _,
                11 => registers[c] = (registers[a] > args[1]) as _,
                12 => registers[c] = (registers[a] > registers[b]) as _,
                13 => registers[c] = (args[0] == registers[b]) as _,
                14 => registers[c] = (registers[a] == args[1]) as _,
                15 => registers[c] = (registers[a] == registers[b]) as _,
                _ => unreachable!(),
            }
            registers
        }

        fn parse_instruction(s: &str) -> (u8, [u32; 3]) {
            let mut nums = s.split(' ');
            let code = next_parse!(nums, u8);
            let args = std::array::from_fn(|_| next_parse!(nums, u32));
            (code, args)
        }

        let mut opcode_map = [None; 16];
        let mut examples = Vec::new();
        let mut lines = input.lines();
        let mut count = 0;
        let sol1 = loop {
            let first = lines.next().unwrap();
            if first.is_empty() {
                break count;
            }
            let mut nums = first["before: [".len()..first.len() - 1].split(", ");
            let before = std::array::from_fn(|_| next_parse!(nums, u32));

            let (code, args) = parse_instruction(lines.next().unwrap());

            let third = lines.next().unwrap();
            let mut nums = third["after:  [".len()..third.len() - 1].split(", ");
            let after = std::array::from_fn(|_| next_parse!(nums, u32));

            let possible_opcodes = (0..16).filter(|&code| {
                exec_op(code, args, before) == after
            }).collect::<Vec<_>>();
            count += (possible_opcodes.len() >= 3) as u32;

            if possible_opcodes.len() == 1 {
                opcode_map[code as usize] = Some(possible_opcodes[0]);
            }
            examples.push((code, possible_opcodes));

            assert_eq!(lines.next(), Some(""));
        };

        let mut done = (0..16).filter_map(|i| opcode_map[i as usize]).collect::<Vec<_>>();
        while let Some(done_code) = done.pop() {
            for (code, possible_opcodes) in examples.iter_mut() {
                if possible_opcodes.len() < 2 {
                    continue;
                }
                if let Some(pos) = possible_opcodes.iter().position(|opcode| *opcode == done_code) {
                    possible_opcodes.remove(pos);
                    if possible_opcodes.len() == 1 && opcode_map[*code as usize].is_none() {
                        opcode_map[*code as usize] = Some(possible_opcodes[0]);
                        done.push(possible_opcodes[0]);
                    }
                }
            }
        }

        assert_eq!(lines.next(), Some(""));
        let sol2 = if opcode_map.iter().any(|entry| entry.is_none()) {
            Solution::None
        } else {
            let mut registers = [0; 4];
            for line in lines {
                let (code, args) = parse_instruction(line);
                let code = opcode_map[code as usize].unwrap();
                registers = exec_op(code, args, registers);
            }
            Solution::U32(registers[0])
        };

        [Solution::U32(sol1), sol2]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1), Solution::None],
            input: "before: [3, 2, 1, 1]
9 2 1 2
after:  [3, 2, 2, 1]



9 2 1 2",
        },
    ];
}
