use crate::Solution;
use std::collections::hash_map::{HashMap, Entry};

pub const SOLUTION: Solution = Solution::U64((3133, 1547953216393));

pub fn solve(input: String) -> Solution {
    let jets = input.into_bytes();
    let jets = &jets[..jets.len() - 1];
    let rocks = [(b'_', 4, 1), (b'+', 3, 3), (b'J', 3, 3), (b'|', 1, 4), (b'o', 2, 2)];
    
    let mut cave = Vec::with_capacity(256);
    let mut h = 0;
    let mut rock_idx = 0;
    let mut jet_idx = 0;
    let mut map = HashMap::new();
    let mut end_at = usize::MAX;
    let mut height1 = 0;
    let mut height2 = 0;
    
    for n in 1.. {
        let (rock, rock_w, rock_h) = rocks[rock_idx];
        let start_h = h + 3 + rock_h;
        if cave.len() < start_h {
            cave.resize(start_h, 0u8);
        }
        
        add_rock(rock, &mut cave, start_h);
        
        let mut rock_pos = (2, start_h - rock_h);
        
        loop {
            match jets[jet_idx] {
                b'>' => {
                    if rock_pos.0 + rock_w != 7 && can_rock_move_right(rock, &cave, rock_pos) {
                        move_rock_right(rock, &mut cave, rock_pos);
                        rock_pos.0 += 1;
                    }
                }
                b'<' => {
                    if rock_pos.0 != 0 && can_rock_move_left(rock, &cave, rock_pos) {
                        move_rock_left(rock, &mut cave, rock_pos);
                        rock_pos.0 -= 1;
                    }
                }
                other => panic!("unknown dir {}", other),
            }
            jet_idx = (jet_idx + 1) % jets.len();
            
            if rock_pos.1 != 0 && can_rock_move_down(rock, &cave, rock_pos) {
                move_rock_down(rock, &mut cave, rock_pos);
                rock_pos.1 -= 1;
            } else {
                break;
            }
        }
        
        h = h.max(rock_pos.1 + rock_h);
        rock_idx = (rock_idx + 1) % rocks.len();
        
        if n == 2022 {
            height1 = h as u64;
            if end_at < 2022 {
                break;
            }
        } else if n > 10000 {
            panic!("found no periodic behaviour after {} rocks, aborting", n);
        }
        
        if rock_idx == 0 && end_at == usize::MAX {
            match map.entry(jet_idx) {
                Entry::Occupied(entry) => {
                    let (old_h, old_n) = entry.get();
                    let old_n = *old_n as u64;
                    let n = n as u64;
                    height2 = ((1000000000000 - old_n) / (n - old_n) - 1) * (h - old_h) as u64;
                    end_at = (n + (1000000000000 - old_n) % (n - old_n)) as usize;
                }
                Entry::Vacant(entry) => { entry.insert((h, n)); }
            }
        }
        
        if n == end_at {
            height2 += h as u64;
            if n >= 2022 {
                break;
            }
        }
    }
    
    (height1, height2).into()
}

fn add_rock(rock: u8, cave: &mut [u8], start_h : usize) {
    match rock {
        b'_' => {
            cave[start_h - 1] = 0b0011110;
        }
        b'+' => {
            cave[start_h - 1] = 0b0001000;
            cave[start_h - 2] = 0b0011100;
            cave[start_h - 3] = 0b0001000;
        }
        b'J' => {
            cave[start_h - 1] = 0b0000100;
            cave[start_h - 2] = 0b0000100;
            cave[start_h - 3] = 0b0011100;
        }
        b'|' => {
            cave[start_h - 1] = 0b0010000;
            cave[start_h - 2] = 0b0010000;
            cave[start_h - 3] = 0b0010000;
            cave[start_h - 4] = 0b0010000;
        }
        b'o' => {
            cave[start_h - 1] = 0b0011000;
            cave[start_h - 2] = 0b0011000;
        }
        other => panic!("unknown rock {}", other),
    }
}

fn can_rock_move_down(rock: u8, cave: &[u8], rock_pos: (usize, usize)) -> bool {
    match rock {
        b'_' => (cave[rock_pos.1 - 1] & (0b1111000 >> rock_pos.0)) == 0,
        b'+' => (cave[rock_pos.1 - 1] & (0b0100000 >> rock_pos.0)) == 0 && (cave[rock_pos.1] & (0b1010000 >> rock_pos.0)) == 0,
        b'J' => (cave[rock_pos.1 - 1] & (0b1110000 >> rock_pos.0)) == 0,
        b'|' => (cave[rock_pos.1 - 1] & (0b1000000 >> rock_pos.0)) == 0,
        b'o' => (cave[rock_pos.1 - 1] & (0b1100000 >> rock_pos.0)) == 0,
        other => panic!("unknown rock {}", other),
    }
}

fn can_rock_move_right(rock: u8, cave: &[u8], rock_pos: (usize, usize)) -> bool {
    match rock {
        b'_' => (cave[rock_pos.1] & (0b0000100 >> rock_pos.0)) == 0,
        b'+' => (cave[rock_pos.1] & (0b0010000 >> rock_pos.0)) == 0
            && (cave[rock_pos.1 + 1] & (0b0001000 >> rock_pos.0)) == 0
            && (cave[rock_pos.1 + 2] & (0b0010000 >> rock_pos.0)) == 0,
        b'J' => cave[rock_pos.1..rock_pos.1 + 3].iter().all(|row| (row & (0b0001000 >> rock_pos.0)) == 0),
        b'|' => cave[rock_pos.1..rock_pos.1 + 4].iter().all(|row| (row & (0b0100000 >> rock_pos.0)) == 0),
        b'o' => (cave[rock_pos.1] & (0b0010000 >> rock_pos.0)) == 0 && (cave[rock_pos.1 + 1] & (0b0010000 >> rock_pos.0)) == 0,
        other => panic!("unknown rock {}", other),
    }
}

fn can_rock_move_left(rock: u8, cave: &[u8], rock_pos: (usize, usize)) -> bool {
    match rock {
        b'_' => (cave[rock_pos.1] & (0b10000000 >> rock_pos.0)) == 0,
        b'+' => (cave[rock_pos.1] & (0b01000000 >> rock_pos.0)) == 0
            && (cave[rock_pos.1 + 1] & (0b10000000 >> rock_pos.0)) == 0
            && (cave[rock_pos.1 + 2] & (0b01000000 >> rock_pos.0)) == 0,
        b'J' => (cave[rock_pos.1] & (0b10000000 >> rock_pos.0)) == 0
            && cave[rock_pos.1 + 1..rock_pos.1 + 3].iter().all(|row| (row & (0b00100000 >> rock_pos.0)) == 0),
        b'|' => cave[rock_pos.1..rock_pos.1 + 4].iter().all(|row| (row & (0b10000000 >> rock_pos.0)) == 0),
        b'o' => (cave[rock_pos.1] & (0b10000000 >> rock_pos.0)) == 0 && (cave[rock_pos.1 + 1] & (0b10000000 >> rock_pos.0)) == 0,
        other => panic!("unknown rock {}", other),
    }
}

fn move_rock_down(rock: u8, cave: &mut [u8], rock_pos: (usize, usize)) {
    match rock {
        b'_' => {
            cave[rock_pos.1 - 1] |= 0b1111000 >> rock_pos.0;
            cave[rock_pos.1] ^= 0b1111000 >> rock_pos.0;
        }
        b'+' => {
            cave[rock_pos.1 - 1] |= 0b0100000 >> rock_pos.0;
            cave[rock_pos.1] |= 0b1010000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b1010000 >> rock_pos.0;
            cave[rock_pos.1 + 2] ^= 0b0100000 >> rock_pos.0;
        }
        b'J' => {
            cave[rock_pos.1 - 1] |= 0b1110000 >> rock_pos.0;
            cave[rock_pos.1] ^= 0b1100000 >> rock_pos.0;
            cave[rock_pos.1 + 2] ^= 0b0010000 >> rock_pos.0;
        }
        b'|' => {
            cave[rock_pos.1 - 1] |= 0b1000000 >> rock_pos.0;
            cave[rock_pos.1 + 3] ^= 0b1000000 >> rock_pos.0;
        }
        b'o' => {
            cave[rock_pos.1 - 1] |= 0b1100000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b1100000 >> rock_pos.0;
        }
        other => panic!("unknown rock {}", other),
    }
}

fn move_rock_right(rock: u8, cave: &mut [u8], rock_pos: (usize, usize)) {
    match rock {
        b'_' => {
            cave[rock_pos.1] ^= 0b1000100 >> rock_pos.0;
        }
        b'+' => {
            cave[rock_pos.1] ^= 0b0110000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b1001000 >> rock_pos.0;
            cave[rock_pos.1 + 2] ^= 0b0110000 >> rock_pos.0;
        }
        b'J' => {
            cave[rock_pos.1] ^= 0b1001000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b0011000 >> rock_pos.0;
            cave[rock_pos.1 + 2] ^= 0b0011000 >> rock_pos.0;
        }
        b'|' => {
            for i in rock_pos.1..rock_pos.1 + 4 {
                cave[i] ^= 0b1100000 >> rock_pos.0;
            }
        }
        b'o' => {
            cave[rock_pos.1] ^= 0b1010000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b1010000 >> rock_pos.0;
        }
        other => panic!("unknown rock {}", other),
    }
}

fn move_rock_left(rock: u8, cave: &mut [u8], rock_pos: (usize, usize)) {
    match rock {
        b'_' => {
            cave[rock_pos.1] ^= 0b10001000 >> rock_pos.0;
        }
        b'+' => {
            cave[rock_pos.1] ^= 0b01100000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b10010000 >> rock_pos.0;
            cave[rock_pos.1 + 2] ^= 0b01100000 >> rock_pos.0;
        }
        b'J' => {
            cave[rock_pos.1] ^= 0b10010000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b00110000 >> rock_pos.0;
            cave[rock_pos.1 + 2] ^= 0b00110000 >> rock_pos.0;
        }
        b'|' => {
            for i in rock_pos.1..rock_pos.1 + 4 {
                cave[i] ^= 0b11000000 >> rock_pos.0;
            }
        }
        b'o' => {
            cave[rock_pos.1] ^= 0b10100000 >> rock_pos.0;
            cave[rock_pos.1 + 1] ^= 0b10100000 >> rock_pos.0;
        }
        other => panic!("unknown rock {}", other),
    }
}
