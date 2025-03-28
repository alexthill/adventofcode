use super::intcode_computer::Comp;
use aoc_lib_rust::{Day, Example, Solution};

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::U64(16209841);
    const PART2: Solution = Solution::U64(8834787);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);
        let sol1 = Comp::new(prog.clone(), [1]).run_to_halt().unwrap() as _;

        let mut comp = Comp::new(prog, [5]);
        comp.exec();
        let sol2 = comp.output().unwrap() as _;

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
