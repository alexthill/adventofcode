use aoc_lib_rust::{Day, Example, Solution};

pub struct Day01;

impl Day for Day01 {

    const PART1: Solution = Solution::U32(1034);
    const PART2: Solution = Solution::U32(1356);

    fn solve(input: &str) -> [Solution; 2] {
        let input = input.as_bytes();
        let mut sol1 = input.windows(2).filter_map(|win| {
            if win[0] == win[1] {
                Some((win[0] - b'0') as u32)
            } else {
                None
            }
        }).sum::<u32>();
        if input[0] == input[input.len() - 1] {
            sol1 += (input[0] - b'0') as u32;
        }

        let sol2 = input.iter().enumerate().filter_map(|(i, c)| {
            if input[(i + input.len() / 2) % input.len()] == *c {
                Some((c - b'0') as u32)
            } else {
                None
            }
        }).sum::<u32>();

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::None],
            input: "1122",
        },
        Example {
            solution: [Solution::U32(4), Solution::None],
            input: "1111",
        },
        Example {
            solution: [Solution::U32(0), Solution::None],
            input: "1234",
        },
        Example {
            solution: [Solution::U32(9), Solution::None],
            input: "91212129",
        },
        Example {
            solution: [Solution::U32(0), Solution::U32(4)],
            input: "12131415",
        },
    ];
}
