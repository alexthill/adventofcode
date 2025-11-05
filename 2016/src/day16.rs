use aoc_lib_rust::{Day, Example, Solution};

pub struct Day16;

impl Day for Day16 {

    const PART1: Solution = Solution::Str("10100011010101011");
    const PART2: Solution = Solution::Str("01010001101011001");

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        fn checksum(input: &[u8], disk: usize) -> String {
            let mut data = input.to_vec();
            while data.len() < disk {
                let cpy = data.iter().copied().rev()
                    .map(|x| x ^ 1)
                    .collect::<Vec<_>>();
                data.push(b'0');
                data.extend(cpy);
            }

            data.truncate(disk);
            while data.len().is_multiple_of(2) {
                for i in 0..data.len() / 2 {
                    data[i] = if data[i * 2] == data [i * 2 + 1] { b'1' } else { b'0' }
                }
                data.truncate(data.len() / 2);
            }

            String::from_utf8(data).unwrap()
        }

        let (sol1, sol2) = if example {
            (checksum(input.as_bytes(), 20), String::new())
        } else {
            (checksum(input.as_bytes(), 272), checksum(input.as_bytes(), 35651584))
        };

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("01100"), Solution::None],
            input: "10000",
        },
    ];
}
