use aoc_lib_rust::{Day, Example, Solution};

pub struct Day02;

impl Day02 {
    fn exec_prog(prog: &mut [u64]) -> u64 {
        let mut ic = 0;
        loop {
            if prog[ic] == 99 {
                break;
            }
            let [a, b] = [1, 2].map(|idx| prog[prog[ic + idx] as usize]);
            let idx = prog[ic + 3] as usize;
            prog[idx] = match prog[ic] {
                1 => a + b,
                2 => a * b,
                _ => unreachable!(),
            };
            ic += 4;
        }
        prog[0]
    }
}

impl Day for Day02 {

    const PART1: Solution = Solution::U64(5482655);
    const PART2: Solution = Solution::U64(4967);

    fn solve(input: &str) -> [Solution; 2] {
        let prog_org = input.split(',')
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<_>>();
        let mut prog = prog_org.clone();

        let sol1 = {
            prog[1] = 12;
            prog[2] = 2;
            Day02::exec_prog(&mut prog)
        };

        let mut sol2 = 0;
        'outer: for noun in 0..99 {
            for verb in 0..99 {
                prog.copy_from_slice(&prog_org);
                prog[1] = noun;
                prog[2] = verb;
                if Day02::exec_prog(&mut prog) == 19690720 {
                    sol2 = 100 * noun + verb;
                    break 'outer;
                }
            }
        }

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
