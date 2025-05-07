use aoc_lib_rust::{Day, Example, Solution};

pub struct Day02;

impl Day for Day02 {

    const PART1: Solution = Solution::U32(4712);
    const PART2: Solution = Solution::Str("lufjygedpvfbhftxiwnaorzmq");

    fn solve(input: &str) -> [Solution; 2] {
        let (count2, count3) = input.as_bytes().split(|c| *c == b'\n').fold((0, 0), |acc, line| {
            let mut counts = [0; 26];
            for c in line {
                counts[(c - b'a') as usize] += 1;
            }
            (
                acc.0 + counts.iter().any(|count| *count == 2) as u32,
                acc.1 + counts.iter().any(|count| *count == 3) as u32,
            )
        });
        let sol1 = count2 * count3;

        let mut sol2 = String::new();
        let mut line_iter = input.as_bytes().split(|c| *c == b'\n');
        while let Some(line) = line_iter.next() {
            'other_loop: for other in line_iter.clone() {
                let mut diff_pos = None;
                for (i, (a, b)) in line.iter().zip(other).enumerate() {
                    if a != b {
                        if diff_pos.is_some() {
                            continue 'other_loop;
                        }
                        diff_pos = Some(i);
                    }
                }
                if let Some(diff_pos) = diff_pos {
                    sol2 = line.into_iter().enumerate().filter_map(|(i, c)| {
                        if i == diff_pos { None } else { Some(*c as char) }
                    }).collect();
                    break;
                }
            }
        }

        [Solution::U32(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(12), Solution::None],
            input: "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab",
        },
        Example {
            solution: [Solution::None, Solution::Str("fgij")],
            input: "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz",
        },
    ];
}
