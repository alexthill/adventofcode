use super::intcode_computer::Comp;
use aoc_lib_rust::{Day, Example, Solution};

pub struct Day09;

impl Day for Day09 {

    const PART1: Solution = Solution::U64(2714716640);
    const PART2: Solution = Solution::U64(58879);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);

        let mut comp = Comp::new(prog.clone(), [1]);
        comp.exec();
        let sol1 = comp.output().unwrap();

        let mut comp = Comp::new(prog, [2]);
        comp.exec();
        let sol2 = comp.output().unwrap();

        [Solution::U64(sol1 as _), Solution::U64(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
