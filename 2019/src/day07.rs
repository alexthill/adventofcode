use super::intcode_computer::Comp;
use aoc_lib_rust::{Day, Example, Solution};

pub struct Day07;

impl Day for Day07 {

    const PART1: Solution = Solution::U32(255590);
    const PART2: Solution = Solution::U32(58285150);

    fn solve(input: &str) -> [Solution; 2] {
        let prog = input.split(',')
            .map(|s| s.parse::<i64>().expect(s))
            .collect::<Vec<_>>();

        fn run1(prog: &[i64], phases: &mut Vec<i64>, input: i64) -> i64 {
            let mut max = i64::MIN;
            for i in 0..=4 {
                if phases.contains(&i) {
                    continue;
                }

                let mut comp = Comp::new(prog.to_vec(), [i, input]);
                comp.exec();
                if phases.len() == 4 {
                    return comp.output().unwrap();
                }
                phases.push(i);
                max = max.max(run1(prog, phases, comp.output().unwrap()));
                phases.pop();
            }
            max
        }
        let sol1 = run1(&prog, &mut Vec::new(), 0);

        fn run2(prog: &[i64], phases: &mut Vec<i64>) -> i64 {
            if phases.len() == 5 {
                let mut comps = [0, 1, 2, 3, 4].map(|i| Comp::new(prog.to_vec(), [phases[i]]));
                let mut last_output = 0;
                let mut curr = 0;
                loop {
                    comps[curr].push_input(last_output);
                    comps[curr].exec();
                    last_output = comps[curr].output().unwrap();
                    if curr == 4 && comps[curr].halted() {
                        break last_output;
                    }
                    curr = (curr + 1) % 5;
                }
            } else {
                (5..10).filter_map(|phase| {
                    if phases.contains(&phase) {
                        None
                    } else {
                        phases.push(phase);
                        let value = run2(prog, phases);
                        phases.pop();
                        Some(value)
                    }
                }).max().unwrap()
            }
        }
        let sol2 = run2(&prog, &mut Vec::new());

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(43210), Solution::None],
            input: "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0",
        },
        Example {
            solution: [Solution::U32(54321), Solution::None],
            input: "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0",
        },
        Example {
            solution: [Solution::U32(65210), Solution::None],
            input: "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0",
        },
        Example {
            solution: [Solution::None, Solution::U32(139629729)],
            input: "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
        },
        Example {
            solution: [Solution::None, Solution::U32(18216)],
            input: "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10",
        },
    ];
}
