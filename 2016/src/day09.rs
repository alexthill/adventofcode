use aoc_lib_rust::{Day, Example, Solution};

pub struct Day09;

impl Day for Day09 {

    const PART1: Solution = Solution::U32(120765);
    const PART2: Solution = Solution::U64(11658395076);

    fn solve(input: &str) -> [Solution; 2] {
        fn parse_num(s: &[u8], idx: &mut usize) -> u64 {
            let mut n = 0;
            while s[*idx].is_ascii_digit() {
                n = 10 * n + (s[*idx] - b'0') as u64;
                *idx += 1;
            }
            n
        }

        fn decompress(s: &[u8], idx: &mut usize) -> [u64; 2] {
            let mut len = [0; 2];
            while *idx < s.len() {
                if s[*idx] == b'(' {
                    *idx += 1;
                    let take = parse_num(s, idx);
                    assert_eq!(s[*idx], b'x');
                    *idx += 1;
                    let repeat = parse_num(s, idx);
                    assert_eq!(s[*idx], b')');
                    *idx += 1;
                    len[0] += repeat * take;
                    len[1] += repeat * decompress(&s[*idx..*idx + take as usize], &mut 0)[1];
                    *idx += take as usize;
                } else {
                    *idx += 1;
                    len[0] += 1;
                    len[1] += 1;
                }
            }
            len
        }

        let [sol1, sol2] = decompress(input.as_bytes(), &mut 0);

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(6), Solution::U32(6)],
            input: "ADVENT",
        },
        Example {
            solution: [Solution::U32(7), Solution::U32(7)],
            input: "A(1x5)BC",
        },
        Example {
            solution: [Solution::U32(18), Solution::U32(20)],
            input: "X(8x2)(3x3)ABCY",
        },
    ];
}
