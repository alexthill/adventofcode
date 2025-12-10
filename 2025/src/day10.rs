use aoc_lib_rust::{Day, Example, Solution};
use microlp::{Problem, OptimizationDirection, ComparisonOp};

pub struct Day10;

impl Day for Day10 {

    const PART1: Solution = Solution::U32(558);
    const PART2: Solution = Solution::U32(20317);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = 0;
        let mut sol2 = 0;
        for line in input.lines() {
            let mut parts = line.split(' ');
            let diagram = parts.next().unwrap();
            let diagram = diagram.as_bytes().iter().skip(1).take(diagram.len() - 2)
                .rfold(0, |acc, x| (acc << 1) | !(*x == b'.') as u16);

            let mut buttons = Vec::new();
            let mut counters = Vec::new();
            for part in parts {
                if part.starts_with('{') {
                    counters = part[1..part.len() - 1].split(',')
                        .map(|n| n.parse::<u16>().unwrap())
                        .collect::<Vec<_>>();
                    break;
                }
                let button = part[1..part.len() - 1].split(',')
                    .map(|n| n.parse::<u16>().unwrap())
                    .fold(0, |acc, n| acc | (1 << n));
                buttons.push(button);
            }
            assert!(buttons.len() <= 16);
            assert!(counters.len() <= 16);

            let mut best = u32::MAX;
            for presses in 0..1_u16 << buttons.len() {
                let state = buttons.iter().enumerate().fold(0, |acc, (i, button)| {
                    acc ^ (button * ((presses >> i) & 1))
                });
                if state == diagram {
                    best = best.min(presses.count_ones());
                }
            }
            sol1 += best;

            let mut problem = Problem::new(OptimizationDirection::Minimize);
            let vars = (0..buttons.len())
                .map(|_| problem.add_integer_var(1.0, (0, i32::MAX)))
                .collect::<Vec<_>>();
            for (i, count) in counters.iter().enumerate() {
                problem.add_constraint(
                    buttons.iter().enumerate().map(|(j, b)| {
                        (vars[j], ((b & (1 << i)) != 0) as u8 as _)
                    }),
                    ComparisonOp::Eq,
                    *count as _,
                );
            }
            let solution = problem.solve().unwrap();
            sol2 += solution.objective().round() as u32;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(7), Solution::U32(33)],
            input: "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}",
        },
    ];
}
