use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::next;

pub struct Day09;

impl Day for Day09 {

    const PART1: Solution = Solution::U32(11898);
    const PART2: Solution = Solution::U32(5601);

    fn solve(input: &str) -> [Solution; 2] {
        fn calc_score(s: &mut impl Iterator<Item = u8>, parent_score: u32) -> (u32, u32) {
            let mut score = parent_score + 1;
            let mut canceled = 0;
            loop {
                match next!(s) {
                    b'{' => {
                        let res = calc_score(s, parent_score + 1);
                        score += res.0;
                        canceled += res.1;
                    }
                    b',' => continue,
                    b'}' => break,
                    b'<' => {
                        loop {
                            match next!(s) {
                                b'>' => break,
                                b'!' => { next!(s); }
                                _ => { canceled += 1; }
                            }
                        }
                    }
                    c => panic!("unexpected char {:?}", char::from_u32(c as u32).unwrap()),
                }
            }
            (score, canceled)
        }

        let mut iter = input.bytes();
        assert_eq!(iter.next(), Some(b'{'));
        let (sol1, sol2) = calc_score(&mut iter, 0);

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(16), Solution::None],
            input: "{{{},{},{{}}}}",
        },
        Example {
            solution: [Solution::U32(9), Solution::None],
            input: "{{<!!>},{<{!!}>},{<!<>},{<!!!<>}}",
        },
        Example {
            solution: [Solution::U32(1), Solution::U32(10)],
            input: "{<{o\"i!a,<{i<a>}",
        },
    ];
}
