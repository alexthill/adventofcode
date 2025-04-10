use super::intcode_computer::{Comp, Interrupt};
use aoc_lib_rust::{Day, Example, Solution};
use aoc_lib_rust::utils::vector::IVec2;
use std::cmp::Reverse;

pub struct Day17;

fn rotr(dir: IVec2) -> IVec2 {
    [-dir[1], dir[0]].into()
}

fn rotl(dir: IVec2) -> IVec2 {
    [dir[1], -dir[0]].into()
}

fn get(map: &[Vec<u8>], pos: IVec2) -> Option<u8> {
    map.get(pos[1] as usize)
        .and_then(|row| row.get(pos[0] as usize))
        .copied()
}

impl Day for Day17 {

    const PART1: Solution = Solution::U32(2660);
    const PART2: Solution = Solution::U32(790595);

    fn solve(input: &str) -> [Solution; 2] {
        let mut prog = Comp::parse_prog(input);
        prog[0] = 2;
        let mut comp = Comp::new(prog, []);
        let mut map = Vec::new();
        let mut line = Vec::new();
        let mut pos = IVec2::default();
        let mut dir = IVec2::default();

        loop {
            match comp.exec() {
                Interrupt::Halt => break,
                Interrupt::Output(10) => {
                    if line.is_empty() {
                        break;
                    } else {
                        map.push(std::mem::take(&mut line));
                    }
                }
                Interrupt::Output(out) => {
                    let old_pos = pos;
                    pos = [line.len() as i32, map.len() as i32].into();
                    line.push(out as u8);
                    match out as u8 {
                        b'^' => dir = [0, -1].into(),
                        b'v' => dir = [0, 1].into(),
                        b'>' => dir = [1, 0].into(),
                        b'<' => dir = [-1, 0].into(),
                        _ => pos = old_pos,
                    }
                }
                _ => unreachable!(),
            }
        }

        let mut sol1 = 0;
        for y in 1..map.len() - 1 {
            for x in 1..map[y].len() - 1 {
                if map[y][x] == b'#'
                    && map[y][x - 1] == b'#' && map[y][x + 1] == b'#'
                    && map[y - 1][x] == b'#' && map[y + 1][x] == b'#'
                {
                    sol1 += (x * y) as u32;
                }
            }
        }

        let mut seq = Vec::new();
        let mut straight = 0;
        loop {
            if let Some(b'#') = get(&map, pos + dir) {
                pos += dir;
                straight += 1;
            } else if let Some(b'#') = get(&map, pos + rotl(dir)) {
                if straight != 0 {
                    seq.push(straight);
                }
                seq.push(b'L');
                dir = rotl(dir);
                straight = 0;
            } else if let Some(b'#') = get(&map, pos + rotr(dir)) {
                if straight != 0 {
                    seq.push(straight);
                }
                seq.push(b'R');
                dir = rotr(dir);
                straight = 0;
            } else {
                seq.push(straight);
                break;
            }
        }

        fn get_function(seq: &mut [u8], letter: u8) -> (usize, Vec<u8>) {
            let min_match_len = 2;
            let mut matches = Vec::new();
            for start in 1..seq.len() - 1 {
                for i in 0.. {
                    if i == 10 || start + i >= seq.len() || seq[start + i] != seq[i] {
                        if i >= min_match_len {
                            matches.push((start, i));
                        }
                        break;
                    }
                }
            }
            if matches.len() < 2 {
                panic!("expected at least 2 matches of length {min_match_len}, found {}", matches.len());
            }
            matches.sort_unstable_by_key(|(_, len)| Reverse(*len));
            let match_len = matches[1].1;
            let mut cutoff = 2;
            while cutoff < matches.len() && matches[cutoff].1 >= match_len {
                cutoff += 1;
            }
            matches.truncate(cutoff);
            for (start, _) in matches {
                for x in &mut seq[start..start + match_len] {
                    *x = letter;
                }
            }
            let mut res = Vec::new();
            for x in &mut seq[..match_len] {
                match *x {
                    x @ (b'L' | b'R') => res.push(x),
                    n if n < 10 => res.push(n + b'0'),
                    n if n < 100 => {
                        res.push(n / 10 + b'0');
                        res.push(n % 10 + b'0');
                    },
                    _ => unreachable!(),
                }
                res.push(b',');
                *x = letter;
            }
            *res.last_mut().unwrap() = b'\n';
            (match_len, res)
        }

        let (a_len, a_fn) = get_function(&mut seq, b'A');
        let b_start = seq.iter().position(|x| *x >= b'L').unwrap();
        let (b_len, b_fn) = get_function(&mut seq[b_start..], b'B');
        let c_start = seq.iter().position(|x| *x >= b'L').unwrap();
        let (c_len, c_fn) = get_function(&mut seq[c_start..], b'C');

        let mut main_fn = Vec::new();
        let mut i = 0;
        while i < seq.len() {
            main_fn.push(seq[i]);
            main_fn.push(b',');
            match seq[i] {
                b'A' => i += a_len,
                b'B' => i += b_len,
                b'C' => i += c_len,
                _ => unreachable!(),
            }
        }
        *main_fn.last_mut().unwrap() = b'\n';

        comp.extend_input(main_fn);
        comp.extend_input(a_fn);
        comp.extend_input(b_fn);
        comp.extend_input(c_fn);
        comp.push_input(b'n' as i64);
        comp.push_input(b'\n' as i64);

        let mut sol2 = 0;
        loop {
            match comp.exec() {
                Interrupt::Halt => break,
                Interrupt::Output(out) => {
                    if out > 127 {
                        sol2 = out as u32;
                    }
                }
                _ => unreachable!(),
            }
        }

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
