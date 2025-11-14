use aoc_lib_rust::{next, next_parse, Day, Example, Solution};

#[derive(Debug, Clone, Copy)]
#[allow(unused)]
enum Op {
    SwapPos(usize, usize),
    SwapLetter(u8, u8),
    RotLeft(usize),
    RotRight(usize),
    RotBased(u8),
    RotBasedInv(u8),
    Rev(usize, usize),
    Move(usize, usize),
}

impl Op {
    fn exec(&self, s: &mut Vec<u8>) {
        match *self {
            Self::SwapPos(x, y) => s.swap(x, y),
            Self::SwapLetter(x, y) => {
                let x = s.iter().position(|el| *el == x).unwrap();
                let y = s.iter().position(|el| *el == y).unwrap();
                s.swap(x, y);
            }
            Self::RotLeft(x) => s.rotate_left(x),
            Self::RotRight(x) => s.rotate_right(x),
            Self::RotBased(x) => {
                let mut x = s.iter().position(|el| *el == x).unwrap();
                x += 1 + (x >= 4) as usize;
                let len = s.len();
                s.rotate_right(x % len);
            }
            Self::RotBasedInv(x) => {
                let mut cpy = s.to_owned();
                for i in 0..s.len() {
                    cpy.copy_from_slice(s);
                    cpy.rotate_left(i);
                    Self::RotBased(x).exec(&mut cpy);
                    if &cpy == s {
                        s.rotate_left(i);
                        return;
                    }
                }
                unreachable!("no inverse of RotBased({x}) found");
            }
            Self::Rev(x, y) => s[x..=y].reverse(),
            Self::Move(x, y) => {
                let letter = s.remove(x);
                s.insert(y, letter);
            }
        }
    }

    fn inverse(&self) -> Self {
        match *self {
            op @ Self::SwapPos(..) => op,
            op @ Self::SwapLetter(..) => op,
            Self::RotLeft(x) => Self::RotRight(x),
            Self::RotRight(x) => Self::RotLeft(x),
            Self::RotBased(x) => Self::RotBasedInv(x),
            Self::RotBasedInv(_) => unreachable!(),
            op @ Self::Rev(..) => op,
            Self::Move(x, y) => Self::Move(y, x),
        }
    }
}

pub struct Day21;

impl Day for Day21 {

    const PART1: Solution = Solution::Str("hcdefbag");
    const PART2: Solution = Solution::Str("fbhaegdc");

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let ops = input.lines().map(|line| {
            let mut parts = line.split(' ');
            match next!(parts) {
                "swap" => match next!(parts) {
                    "position" => {
                        let x = next_parse!(parts, usize);
                        assert_eq!(next!(parts), "with");
                        assert_eq!(next!(parts), "position");
                        let y = next_parse!(parts, usize);
                        Op::SwapPos(x, y)
                    }
                    "letter" => {
                        let x = next!(parts).as_bytes()[0];
                        assert_eq!(next!(parts), "with");
                        assert_eq!(next!(parts), "letter");
                        let y = next!(parts).as_bytes()[0];
                        Op::SwapLetter(x, y)
                    }
                    _ => unreachable!(),
                }
                "rotate" => match next!(parts) {
                    "left" => Op::RotLeft(next_parse!(parts, usize)),
                    "right" => Op::RotRight(next_parse!(parts, usize)),
                    "based" => {
                        assert_eq!(next!(parts), "on");
                        assert_eq!(next!(parts), "position");
                        assert_eq!(next!(parts), "of");
                        assert_eq!(next!(parts), "letter");
                        let x = next!(parts).as_bytes()[0];
                        Op::RotBased(x)
                    }
                    _ => unreachable!(),
                }
                "reverse" => {
                    assert_eq!(next!(parts), "positions");
                    let x = next_parse!(parts, usize);
                    assert_eq!(next!(parts), "through");
                    let y = next_parse!(parts, usize);
                    Op::Rev(x, y)
                }
                "move" => {
                    assert_eq!(next!(parts), "position");
                    let x = next_parse!(parts, usize);
                    assert_eq!(next!(parts), "to");
                    assert_eq!(next!(parts), "position");
                    let y = next_parse!(parts, usize);
                    Op::Move(x, y)
                }
                _ => unreachable!(),
            }
        }).collect::<Vec<_>>();


        let mut pass = if example {
            b"abcde".to_vec()
        } else {
            b"abcdefgh".to_vec()
        };
        for op in ops.iter() {
            op.exec(&mut pass);
        }
        let sol1 = String::from_utf8(pass).unwrap();

        let mut pass = if example {
            b"decab".to_vec()
        } else {
            b"fbgdceah".to_vec()
        };
        for op in ops.iter().rev() {
            op.inverse().exec(&mut pass);
        }
        let sol2 = String::from_utf8(pass).unwrap();

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("decab"), Solution::Str("abcde")],
            input: "swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d",
        },
    ];
}
