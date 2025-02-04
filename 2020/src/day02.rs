use aoc_lib_rust::{Day, Example, Solution};

pub struct Day02;

impl Day for Day02 {

    const PART1: Solution = Solution::U32(622);
    const PART2: Solution = Solution::U32(263);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = 0;
        let mut sol2 = 0;

        for line in input.split('\n') {
            let mut parts = line.split(['-', ' ']);
            let min = parts.next().unwrap().parse::<usize>().unwrap();
            let max = parts.next().unwrap().parse::<usize>().unwrap();
            let letter = parts.next().unwrap().bytes().next().unwrap();
            let sequence = parts.next().unwrap().as_bytes();
            let count = sequence.iter().filter(|c| **c == letter).count();

            sol1 += (min <= count && count <= max) as u32;
            sol2 += ((sequence[min - 1] == letter) != (sequence[max - 1] == letter)) as u32;
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::U32(1)],
            input: "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc",
        },
    ];
}
