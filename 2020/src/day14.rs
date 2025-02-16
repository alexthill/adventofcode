use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashMap;

pub struct Day14;

impl Day14 {
    fn get_masks(line: &[u8]) -> [u64; 2] {
        line.iter().fold([0, 0], |masks, c| {
            let mut masks = [masks[0] << 1, masks[1] << 1];
            match *c {
                b'0' => masks[0] |= 1,
                b'1' => masks[1] |= 1,
                other => debug_assert!(other == b'X', "{other}"),
            }
            masks
        })
    }

    fn insert_values(mem: &mut HashMap<u64, u64>, adr: u64, val: u64, mask: &[u8], i: usize) {
        match mask.get(i) {
            None => {
                mem.insert(adr, val);
            }
            Some(b'X') => {
                let m = 1 << (mask.len() - i - 1);
                Self::insert_values(mem, adr | m, val, mask, i + 1);
                Self::insert_values(mem, adr & !m, val, mask, i + 1);
            }
            _ => {
                Self::insert_values(mem, adr, val, mask, i + 1);
            }
        }
    }
}

impl Day for Day14 {

    const PART1: Solution = Solution::U64(12408060320841);
    const PART2: Solution = Solution::U64(4466434626828);

    fn solve2(input: &str, is_example: bool) -> [Solution; 2] {
        let mut mem1 = HashMap::new();
        let mut mem2 = HashMap::new();
        let mut mask = &[][..];
        let mut masks = [0, 0];

        for line in input.as_bytes().split(|c| *c == b'\n') {
            if line[1] == b'a' {
                mask = &line[7..];
                masks = Day14::get_masks(&line[7..]);
                continue;
            }

            let mut iter = line.iter().skip(4);
            let mut adr = 0;
            for c in iter.by_ref() {
                if !c.is_ascii_digit() {
                    break;
                }
                adr = adr * 10 + (*c - b'0') as u64;
            }
            let val = iter.skip(3).fold(0, |acc, c| acc * 10 + (*c - b'0') as u64);
            mem1.insert(adr, (val | masks[1]) & !masks[0]);
            if is_example && mask.iter().filter(|c| **c == b'X').count() > 10 {
                continue;
            }
            Day14::insert_values(&mut mem2, adr | masks[1], val, mask, 0);
        }

        [Solution::U64(mem1.values().sum()), Solution::U64(mem2.values().sum())]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(165), Solution::None],
            input: "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0",
        },
        Example {
            solution: [Solution::None, Solution::U32(208)],
            input: "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1",
        },
    ];
}
