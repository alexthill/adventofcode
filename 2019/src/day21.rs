use aoc_lib_rust::{Day, Example, Solution};
use super::intcode_computer::{Comp, Interrupt};

pub struct Day21;

impl Day for Day21 {

    const PART1: Solution = Solution::U64(19354464);
    const PART2: Solution = Solution::U64(1143198454);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);

        fn run<const N: usize>(prog: Vec<i64>, ascii_prog: [i64; N]) -> u64 {
            let mut comp = Comp::new(prog, ascii_prog);
            loop {
                match comp.exec() {
                    Interrupt::Output(out) if out > u8::MAX as i64 => break out as u64,
                    Interrupt::Output(_out) => {
                        // print!("{}", char::from_u32(_out as u32).unwrap());
                    }
                    Interrupt::Halt => break 0,
                    _ => unreachable!(),
                }
            }
        }

        let ascii_prog = b"NOT J J
AND A J
AND B J
AND C J
NOT J J
AND D J
WALK
".map(|c| c as i64);
        let sol1 = run(prog.clone(), ascii_prog);

        let ascii_prog = b"NOT J J
AND A J
AND B J
AND C J
NOT J J
AND D J
OR E T
OR H T
AND T J
RUN
".map(|c| c as i64);
        let sol2 = run(prog, ascii_prog);

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
