use aoc_lib_rust::{Day, Example, Solution};

pub struct Day18;

impl Day18 {
    fn calc_part1(iter: &mut impl Iterator<Item=u8>) -> u64 {
        let mut res = match iter.next() {
            Some(b'(') => Self::calc_part1(iter),
            Some(c) => (c - b'0') as _,
            None => unreachable!(),
        };
        loop {
            let op = match iter.next() {
                Some(b')') | None => break,
                Some(op) => op,
            };
            let num = match iter.next() {
                Some(b'(') => Self::calc_part1(iter),
                Some(c) => (c - b'0') as _,
                None => unreachable!(),
            };
            match op {
                b'+' => res += num,
                b'*' => res *= num,
                _ => unreachable!(),
            }
        }
        res
    }

    fn calc_part2(iter: &mut impl Iterator<Item=u8>) -> u64 {
        let first = match iter.next() {
            Some(b'(') => Self::calc_part2(iter),
            Some(c) => (c - b'0') as _,
            None => unreachable!(),
        };
        let mut stack = vec![first];
        loop {
            let op = match iter.next() {
                Some(b')') | None => break,
                Some(op) => op,
            };
            let num = match iter.next() {
                Some(b'(') => Self::calc_part2(iter),
                Some(c) => (c - b'0') as _,
                None => unreachable!(),
            };
            match op {
                b'+' => {
                    let a = stack.pop().unwrap();
                    stack.push(a + num);
                }
                b'*' => stack.push(num),
                _ => unreachable!(),
            }
        }
        stack.into_iter().product()
    }
}

impl Day for Day18 {

    const PART1: Solution = Solution::U64(18213007238947);
    const PART2: Solution = Solution::U64(388966573054664);

    fn solve(input: &str) -> [Solution; 2] {
        let (sol1, sol2) = input.as_bytes().split(|c| *c == b'\n').map(|line| {
            let mut iter = line.iter().copied().filter(|c| !c.is_ascii_whitespace());
            let sol1 = Day18::calc_part1(&mut iter.clone());
            let sol2 = Day18::calc_part2(&mut iter);
            (sol1, sol2)
        })
        .reduce(|acc, x| (acc.0 + x.0, acc.1 + x.1))
        .unwrap();

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(71), Solution::U32(231)],
            input: "1 + 2 * 3 + 4 * 5 + 6",
        },
        Example {
            solution: [Solution::U32(51), Solution::U32(51)],
            input: "1 + (2 * 3) + (4 * (5 + 6))",
        },
        Example {
            solution: [Solution::U32(13632), Solution::U32(23340)],
            input: "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",
        },
    ];
}
