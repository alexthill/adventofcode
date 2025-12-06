use aoc_lib_rust::{Day, Example, Solution};

pub struct Day06;

impl Day for Day06 {

    const PART1: Solution = Solution::U64(4412382293768);
    const PART2: Solution = Solution::U64(7858808482092);

    fn solve(input: &str) -> [Solution; 2] {
        let last_line = input.lines().last().unwrap();
        assert!(
            input.lines().all(|line| line.len() == last_line.len()),
            "expected all lines to have the same length",
        );
        let line_len = last_line.len() + 1; // +1 for newline
        let line_count = input.lines().count() - 1; // -1 for line with operators
        let operators = last_line
            .split_whitespace()
            .map(|part| part.bytes().next().unwrap())
            .collect::<Vec<_>>();

        let sol1 = {
            let mut results = operators.iter()
                .map(|op| (*op == b'*') as u64)
                .collect::<Vec<_>>();
            for line in input.lines().take(line_count) {
                for (i, part) in line.split_whitespace().enumerate() {
                    let num = part.parse::<u64>().unwrap();
                    match operators[i] {
                        b'+' => results[i] += num,
                        b'*' => results[i] *= num,
                        _ => unreachable!(),
                    }
                }
            }
            results.into_iter().sum()
        };

        let sol2 = {
            let input = input.as_bytes();
            let mut results = operators.iter()
                .map(|op| (*op == b'*') as u64)
                .collect::<Vec<_>>();
            let mut i = 0;
            for col in 0..line_len - 1 {
                let mut num = 0;
                for row in 0..line_count {
                    let idx = col + line_len * row;
                    if input[idx].is_ascii_digit() {
                        num = num * 10 + (input[idx] - b'0') as u64;
                    }
                }
                if num == 0 {
                    i += 1;
                } else {
                    match operators[i] {
                        b'+' => results[i] += num,
                        b'*' => results[i] *= num,
                        _ => unreachable!(),
                    }
                }
            }
            results.into_iter().sum()
        };

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(4277556), Solution::U32(3263827)],
            input: "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ",
        },
    ];
}
