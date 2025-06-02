use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next, next_parse};

pub struct Day19;

impl Day for Day19 {

    const PART1: Solution = Solution::U32(2640);
    const PART2: Solution = Solution::U32(27024480);

    fn solve(input: &str) -> [Solution; 2] {
        let mut lines = input.lines();
        let (_, n) = next!(lines).split_once(' ').unwrap();
        let ip = n.parse::<usize>().unwrap();
        let instructions = lines.map(|line| {
            let mut parts = line.split(' ');
            let code = match next!(parts) {
                "addr" => 0,
                "addi" => 1,
                "mulr" => 2,
                "muli" => 3,
                "banr" => 4,
                "bani" => 5,
                "borr" => 6,
                "bori" => 7,
                "setr" => 8,
                "seti" => 9,
                "gtir" => 10,
                "gtri" => 11,
                "gtrr" => 12,
                "eqir" => 13,
                "eqri" => 14,
                "eqrr" => 15,
                _ => unreachable!(),
            };
            let args = [next_parse!(parts, u32), next_parse!(parts, u32), next_parse!(parts, u32)];
            (code, args)
        }).collect::<Vec<_>>();

        fn exec_op(code: u8, args: [u32; 3], mut registers: [u32; 6]) -> [u32; 6] {
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

        fn get_num(instructions: &[(u8, [u32; 3])], ip: usize, mut registers: [u32; 6]) -> u32 {
            let mut old_max = 0;
            let mut old_max_age = 0;
            while let Some((code, args)) = instructions.get(registers[ip] as usize) {
                registers = exec_op(*code as u8, *args, registers);
                registers[ip] += 1;

                let max = registers.into_iter().max().unwrap();
                if max > old_max {
                    old_max = max;
                    old_max_age = 0;
                } else if old_max_age > 10 {
                    return old_max;
                }
                old_max_age += 1;
            };
            unreachable!()
        }

        fn sum_divisors(num: u32) -> u32 {
            let to = (num as f32).sqrt().ceil() as u32;
            (1..to).filter_map(|n| {
                if num % n == 0 { Some(n + num / n) } else { None }
            }).sum::<u32>() + if to * to == num { to } else { 0 }
        }

        let num = get_num(&instructions, ip, [0; 6]);
        let sol1 = sum_divisors(num);

        let num = get_num(&instructions, ip, [1, 0, 0, 0, 0, 0]);
        let sol2 = sum_divisors(num);

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        /*
        Example {
            solution: [Solution::U32(7), Solution::None],
            input: "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5",
        },
        */
    ];
}
