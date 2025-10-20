use aoc_lib_rust::{Day, Example, Solution};

pub struct Day05;

impl Day for Day05 {

    const PART1: Solution = Solution::Str("c6697b55");
    const PART2: Solution = Solution::Str("8c35d1ab");

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = String::new();
        let mut sol2 = [None; 8];
        for i in 0.. {
            let s = format!("{input}{i}");
            let h = md5::compute(s.as_bytes());

            if h[0] != 0 || h[1] != 0 || (h[2] & 0xf0) != 0 {
                continue;
            }

            let a = h[2] & 0x0f;
            let b = h[3] >> 4;
            if sol1.len() < 8 {
                sol1.push(char::from_digit(a as _, 16).unwrap());
            }
            if a < 8 {
                let x = &mut sol2[a as usize];
                if x.is_none() {
                    *x = Some(char::from_digit(b as _, 16).unwrap());
                    if sol2.iter().all(|x| x.is_some()) {
                        break;
                    }
                }
            }
        }
        let sol2 = sol2.into_iter().flatten().collect();

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("18f47a30"), Solution::Str("05ace8e3")],
            input: "abc",
        },
    ];
}
