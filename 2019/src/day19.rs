use aoc_lib_rust::{Day, Example, Solution};
use super::intcode_computer::{Comp, Interrupt};

pub struct Day19;

impl Day for Day19 {

    const PART1: Solution = Solution::U32(173);
    const PART2: Solution = Solution::U32(6671097);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = Comp::parse_prog(input);
        let mut comp = Comp::new(prog.clone(), []);

        let mut sol1 = 1;
        let mut x_min = 0;
        for y in 1..50 {
            let mut found = false;
            for x in x_min.. {
                comp.reset(&prog);
                comp.extend_input([x, y]);
                match comp.exec() {
                    Interrupt::Output(0) => {
                        if found || x - x_min > 10 {
                            break;
                        }
                    }
                    Interrupt::Output(1) => {
                        sol1 += 1;
                        if !found {
                            found = true;
                            x_min = x;
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }

        let mut x_min = 0;
        for x in 1.. {
            comp.reset(&prog);
            comp.extend_input([x, 100]);
            match comp.exec() {
                Interrupt::Output(0) => {}
                Interrupt::Output(1) => {
                    x_min = x;
                    break;
                }
                _ => unreachable!(),
            }
        }

        let mut sol2 = 0;
        'y_loop: for y in 100.. {
            for x in x_min.. {
                comp.reset(&prog);
                comp.extend_input([x, y]);
                match comp.exec() {
                    Interrupt::Output(0) => {}
                    Interrupt::Output(1) => {
                        x_min = x;
                        comp.reset(&prog);
                        comp.extend_input([x + 99, y - 99]);
                        match comp.exec() {
                            Interrupt::Output(0) => break,
                            Interrupt::Output(1) => {
                                sol2 = x * 10000 + y - 99;
                                break 'y_loop;
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
