use crate::Solution;

pub const SOLUTION: Solution = Solution::U32((4208, 1016));

struct Elf {
    id: u16,
    pos: usize,
    to: usize,
}

pub fn solve(input: String) -> Solution {
    const ID_OFFSET: u16 = 2;
    
    let w = input.bytes().position(|c| c == b'\n').unwrap();
    let h = input.as_bytes().len() / (w + 1);
    assert_eq!(w, h);
    
    let padding = w;
    let w = w + 2 * padding;
    let h = h + 2 * padding;
    let mut elves = Vec::new();
    let mut map = Vec::new();
    map.resize(w * h, 0);
    
    let mut i = w * padding + padding;
    for el in input.bytes() {
        match el {
            b'.' => i += 1,
            b'#' => {
                elves.push(Elf { id: elves.len() as u16 + ID_OFFSET, pos: i, to: 0 });
                map[i] = 1;
                i += 1;
            }
            b'\n' => i += 2 * padding,
            other => panic!("unexpected char {}", other),
        }
    }
    
    let dirs = [-(w as i32), w as i32, -1, 1];
    let mut res1 = 0;
    let mut res2 = 0;
    for round in 0..10000 {
        for i in 0..elves.len() {
            let pos = elves[i].pos;
            let (tl, tm, tr) = (map[pos - w - 1] != 1, map[pos - w] != 1, map[pos - w + 1] != 1);
            let (ml, mr) = (map[pos - 1] != 1, map[pos + 1] != 1);
            let (bl, bm, br) = (map[pos + w - 1] != 1, map[pos + w] != 1, map[pos + w + 1] != 1);
            let free = [
                tl && tm && tr,
                bl && bm && br,
                tl && ml && bl,
                tr && mr && br,
            ];
            if !free.iter().all(|&x| x) {
                for f in (0..4).map(|x| (x + round) % 4) {
                    if free[f] {
                        let new_pos = (pos as i32 + dirs[f]) as usize;
                        if map[new_pos] == 0 {
                            map[new_pos] = elves[i].id;
                            elves[i].to = new_pos;
                        } else {
                            elves[(map[new_pos] - ID_OFFSET) as usize].to = 0;
                            map[new_pos] = 0;
                        }
                        break;
                    }
                }
            }
        }
        
        let mut some_elf_moved = false;
        for elf in elves.iter_mut() {
            if elf.to != 0 {
                map[elf.pos] = 0;
                map[elf.to] = 1;
                elf.pos = elf.to;
                elf.to = 0;
                some_elf_moved = true;
            }
        }
        
        if !some_elf_moved {
            res2 = round + 1;
            break;
        }
        
        if round == 9 {
            let mut x_min = usize::MAX;
            let mut x_max = 0;
            let mut y_min = usize::MAX;
            let mut y_max = 0;
            for elf in elves.iter() {
                let (x, y) = (elf.pos % w, elf.pos / w);
                x_min = x_min.min(x);
                x_max = x_max.max(x);
                y_min = y_min.min(y);
                y_max = y_max.max(y);
            }
            res1 = (x_max - x_min + 1) * (y_max - y_min + 1) - elves.len();
        }
    }
    
    (res1 as u32, res2 as u32).into()
}
