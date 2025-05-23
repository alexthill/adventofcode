use aoc_lib_rust::{Day, Example, Solution};

pub struct Day14;

impl Day for Day14 {

    const PART1: Solution = Solution::Str("3410710325");
    const PART2: Solution = Solution::U32(20216138);

    fn solve(input: &str) -> [Solution; 2] {
        let n = input.parse().unwrap();
        let find = input.bytes().map(|c| c - b'0').collect::<Vec<_>>();

        let mut scores: Vec<u8> = vec![3, 7];
        scores.reserve(n);
        let mut idx = [0, 1];
        let mut sol2 = 0;
        while scores.len() < n + 10 || sol2 == 0 {
            let sum = scores[idx[0]] + scores[idx[1]];
            if sum > 9 {
                scores.push(sum / 10);
                scores.push(sum % 10);
            } else {
                scores.push(sum);
            }
            for idx in idx.iter_mut() {
                *idx = (*idx + 1 + scores[*idx] as usize) % scores.len();
            }
            if scores.len() > find.len() {
                let start = scores.len() - find.len();
                if scores[start..] == find {
                    sol2 = start;
                } else if scores[start - 1..scores.len() - 1] == find {
                    sol2 = start - 1;
                }
            }
        }

        let sol1 = scores[n..n + 10].iter()
            .map(|score| (*score + b'0') as char)
            .collect::<String>();

        [Solution::String(sol1), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("5158916779"), Solution::None],
            input: "9",
        },
        Example {
            solution: [Solution::Str("3910137144"), Solution::U32(9)],
            input: "51589",
        },
    ];
}
