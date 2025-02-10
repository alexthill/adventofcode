use aoc_lib_rust::{Day, Example, Solution};

pub struct Day11;

impl Day11 {
    fn next1(w: isize, old: &[u8], new: &mut [u8]) -> bool {
        let mut change = false;
        for (i, &c) in old.iter().enumerate() {
            if c == b'L' || c == b'#' {
                let occ = [1, 1 - w, 1 + w, -1, -1 - w, -1 + w, w, -w].into_iter()
                    .filter(|offset| {
                        (i as isize + offset).try_into().ok()
                            .and_then(|p: usize| old.get(p))
                            .map(|c| *c == b'#')
                            .unwrap_or(false)
                    })
                    .count();
                if c == b'L' && occ == 0 {
                    new[i] = b'#';
                    change = true;
                    continue;
                }
                if c == b'#' && occ >= 4 {
                    new[i] = b'L';
                    change = true;
                    continue;
                }
            }
            new[i] = old[i];
        }
        change
    }

    fn next2(w: isize, old: &[u8], new: &mut [u8]) -> bool {
        let mut change = false;
        for (i, &c) in old.iter().enumerate() {
            if c == b'L' || c == b'#' {
                let occ = [1, 1 - w, 1 + w, -1, -1 - w, -1 + w, w, -w].into_iter()
                    .filter(|offset| {
                        let mut pos = i as isize + offset;
                        loop {
                            let c = pos.try_into().ok().and_then(|p: usize| old.get(p));
                            pos += offset;
                            match c {
                                Some(b'.') => continue,
                                Some(b'#') => break true,
                                _ => break false,
                            }
                        }
                    })
                    .count();
                if c == b'L' && occ == 0 {
                    new[i] = b'#';
                    change = true;
                    continue;
                }
                if c == b'#' && occ >= 5 {
                    new[i] = b'L';
                    change = true;
                    continue;
                }
            }
            new[i] = old[i];
        }
        change
    }
}

impl Day for Day11 {

    const PART1: Solution = Solution::U32(2310);
    const PART2: Solution = Solution::U32(2074);

    fn solve(input: &str) -> [Solution; 2] {
        let input = input.as_bytes();
        let w = 1 + input.iter().position(|c| *c == b'\n').unwrap();

        let mut copy_a: Box<[u8]> = input.into();
        let mut copy_b: Box<[u8]> = input.into();
        loop {
            if !Self::next1(w as isize, &copy_a, &mut copy_b) {
                break;
            }
            std::mem::swap(&mut copy_a, &mut copy_b);
        }
        let sol1 = copy_a.iter().filter(|c| **c == b'#').count();

        let mut copy_a: Box<[u8]> = input.into();
        let mut copy_b: Box<[u8]> = input.into();
        loop {
            if !Self::next2(w as isize, &copy_a, &mut copy_b) {
                break;
            }
            std::mem::swap(&mut copy_a, &mut copy_b);
        }
        let sol2 = copy_a.iter().filter(|c| **c == b'#').count();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(37), Solution::U32(26)],
            input: "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL",
        },
    ];
}
