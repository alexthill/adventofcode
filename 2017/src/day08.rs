use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::{next, next_parse};
use std::collections::HashMap;

pub struct Day08;

impl Day for Day08 {

    const PART1: Solution = Solution::U32(3089);
    const PART2: Solution = Solution::U32(5391);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol2 = i32::MIN;
        let mut regs = HashMap::new();
        for line in input.lines() {
            let mut parts = line.split(' ');
            let reg = next!(parts);
            let op = next!(parts);
            let val = next_parse!(parts, i32);
            assert_eq!(next!(parts), "if");
            let cmp_reg = regs.get(next!(parts)).copied().unwrap_or(0);
            let cmp_op = next!(parts);
            let cmp_val = next_parse!(parts, i32);

            let val = match op {
                "inc" => val,
                "dec" => -val,
                _ => unreachable!(),
            };
            let do_op = match cmp_op {
                "==" => cmp_reg == cmp_val,
                "!=" => cmp_reg != cmp_val,
                "<=" => cmp_reg <= cmp_val,
                ">=" => cmp_reg >= cmp_val,
                "<" => cmp_reg < cmp_val,
                ">" => cmp_reg > cmp_val,
                _ => unreachable!(),
            };

            if do_op {
                let reg_val = regs.entry(reg).or_insert(0);
                *reg_val += val;
                sol2 = sol2.max(*reg_val);
            }
        }

        let sol1 = *regs.values().max().unwrap();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(1), Solution::U32(10)],
            input: "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10",
        },
    ];
}
