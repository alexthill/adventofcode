use aoc_lib_rust::{next, Day, Example, Solution};

#[derive(Debug, Clone, Copy)]
enum Move {
    S(usize),
    X(usize, usize),
    P(u8, u8),
}

pub struct Day16;

impl Day for Day16 {

    const PART1: Solution = Solution::Str("iabmedjhclofgknp");
    const PART2: Solution = Solution::Str("oildcmfeajhbpngk");

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let moves = input.as_bytes().split(|c| *c == b',').map(|mv| {
            match mv[0] {
                b's' => Move::S(mv[1..].iter().fold(0, |acc, x| acc * 10 + (*x - b'0') as usize)),
                b'x' => {
                    let mut parts = mv[1..].split(|c| *c == b'/').map(|part| {
                        part.iter().fold(0, |acc, x| acc * 10 + (*x - b'0') as usize)
                    });
                    Move::X(next!(parts), next!(parts))
                }
                b'p' => Move::P(mv[1], mv[3]),
                _ => unreachable!(),
            }
        }).collect::<Vec<_>>();

        fn dance(progs: &mut [u8], start: &mut usize, moves: &[Move]) {
            let len = progs.len();
            for &mv in moves {
                match mv {
                    Move::S(spin) => {
                        if *start < spin {
                            *start += len;
                        }
                        *start -= spin;
                    }
                    Move::X(a, b) => progs.swap((a + *start) % len, (b + *start) % len),
                    Move::P(a, b) => {
                        let a = progs.iter().position(|c| *c == a).unwrap();
                        let b = progs.iter().position(|c| *c == b).unwrap();
                        progs.swap(a, b);
                    }
                }
            }
        }

        fn progs_to_string(progs: &[u8], start: usize) -> String {
            progs[start..].iter().chain(&progs[..start])
                .map(|c| char::from_u32(*c as _).unwrap())
                .collect()
        }

        let mut progs = (b'a'..=if example { b'e' } else { b'p' }).collect::<Box<[_]>>();
        let mut start = 0;
        dance(&mut progs, &mut start, &moves);
        let sol1 = progs_to_string(&progs, start);

        let mut i = 0;
        let sol2 = loop {
            i += 1;
            if progs[start..].iter().chain(&progs[..start]).copied()
                .eq(b'a'..=if example { b'e' } else { b'p' })
            {
                for _ in 0..1_000_000_000 % i {
                    dance(&mut progs, &mut start, &moves);
                }
                break progs_to_string(&progs, start);
            }
            dance(&mut progs, &mut start, &moves);
        };

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("baedc"), Solution::Str("abcde")],
            input: "s1,x3/4,pe/b",
        },
    ];
}
