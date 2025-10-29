use aoc_lib_rust::{Day, Example, Solution};
use std::collections::VecDeque;

pub struct Day14;

impl Day for Day14 {

    const PART1: Solution = Solution::U32(18626);
    const PART2: Solution = Solution::U32(20092);

    fn solve(input: &str) -> [Solution; 2] {
        fn compute_hash_properties(hash: md5::Digest) -> Option<(u8, [bool; 16])> {
            let mut nibbles = [0; 32];
            for (h, [a, b]) in hash.0.into_iter().zip(nibbles.as_chunks_mut().0) {
                *a = h >> 4;
                *b = h & 0x0f;
            }

            let mut triple = None;
            for window in nibbles.windows(3) {
                if window[0] == window[1] && window[1] == window[2] {
                    triple = Some(window[0]);
                    break;
                }
            }

            let triple = triple?;
            let mut pentuples = [false; 16];
            for window in nibbles.windows(5) {
                if window.iter().skip(1).all(|&x| x == window[0]) {
                    pentuples[window[0] as usize] = true;
                }
            }
            Some((triple, pentuples))
        }

        fn find_sol<F: Fn(&[u8]) -> md5::Digest>(input: &str, hasher: F) -> u32 {
            let mut hash_properties = VecDeque::new();
            for i in 0..1000 {
                let s = format!("{input}{i}");
                let props = compute_hash_properties(hasher(s.as_bytes()));
                hash_properties.push_back(props);
            }

            let mut count = 0;
            let mut i = 0;
            loop {
                let s = format!("{input}{}", i + hash_properties.len());
                let props = compute_hash_properties(hasher(s.as_bytes()));
                hash_properties.push_back(props);
                i += 1;

                let Some(props) = hash_properties.pop_front().unwrap() else { continue };
                let n = props.0;

                for props in hash_properties.iter() {
                    let Some(props) = props else { continue };
                    if props.1[n as usize] {
                        count += 1;
                        break;
                    }
                }

                if count == 64 {
                    return (i - 1) as _;
                }
            }
        }

        fn to_hex_digit(n: u8) -> u8 {
            if n < 10 { n + b'0' } else { n - 10 + b'a' }
        }

        let sol1 = find_sol(input, |x| md5::compute(x));
        let sol2 = find_sol(input, |x| {
            let mut digest = md5::compute(x);
            for _ in 0..2016 {
                let mut input = [0; 32];
                for (h, [a, b]) in digest.0.into_iter().zip(input.as_chunks_mut().0) {
                    *a = to_hex_digit(h >> 4);
                    *b = to_hex_digit(h & 0x0f);
                }
                digest = md5::compute(input);
            }
            digest
        });

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(22728), Solution::U32(22551)],
            input: "abc",
        },
    ];
}
