use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::modular_inverse;

#[derive(Debug, Clone, Copy)]
enum Move {
    DealIntoNew,
    DealWithIncr(i64),
    Cut(i64),
}

pub struct Day22;

impl Day for Day22 {

    const PART1: Solution = Solution::U32(2604);
    const PART2: Solution = Solution::U64(79608410258462);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let moves = input.lines().map(|line| {
            if line == "deal into new stack" {
                Move::DealIntoNew
            } else if line.starts_with("cut") {
                let cut = line["cut ".len()..].parse::<i64>().unwrap();
                Move::Cut(cut)
            } else if line.starts_with("deal with increment") {
                let incr = line["deal with increment ".len()..].parse::<i64>().unwrap();
                Move::DealWithIncr(incr)
            } else {
                unreachable!();
            }
        }).collect::<Vec<_>>();

        fn reduce(moves: &[Move], size: i64) -> (i64, i64) {
            let (mut a, mut b) = (1, 0);
            for mov in moves.iter().copied() {
                match mov {
                    Move::DealIntoNew => (a, b) = (size - a, size - 1 - b),
                    Move::DealWithIncr(incr) => (a, b) = ((a * incr) % size, (b * incr) % size),
                    Move::Cut(cut) => b = b - cut,
                }
            }
            (a, b)
        }

        let size = if example { 10 } else { 10007 };
        let pos = if example { 9 } else { 2019 };
        let (a, b) = reduce(&moves, size);
        let sol1 = (a * pos + b) % size;

        fn linear_exp(a: i128, b: i128, e: i128, m: i128) -> (i128, i128) {
            if e == 1 {
                (a, b)
            } else if e % 2 == 0 {
                linear_exp((a * a) % m, (a * b + b) % m, e / 2, m)
            } else {
                let (c, d) = linear_exp(a, b, e - 1, m);
                ((a * c) % m, (a * d + b) % m)
            }
        }

        let size = if example { 10 } else { 119315717514047 };
        let pos = if example { 9 } else { 2020 };
        let times = if example { 5 } else { 101741582076661 };
        let (a, b) = reduce(&moves, size);
        let a_inv = modular_inverse(a, size).unwrap() as i128;
        let b_inv = (-b as i128 * a_inv) % size as i128;
        let (a_exp, b_exp) = linear_exp(a_inv, b_inv, times, size as _);
        let sol2 = (a_exp * pos + b_exp) % size as i128;

        [Solution::U32(sol1 as _), Solution::U64(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(3), Solution::U32(7)],
            input: "deal with increment 7
deal into new stack
deal into new stack",
        },
        Example {
            solution: [Solution::U32(0), Solution::U32(6)],
            input: "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1",
        },
    ];
}
