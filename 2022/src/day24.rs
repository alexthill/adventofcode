use crate::Solution;
use std::collections::{HashSet, VecDeque};

pub const SOLUTION: Solution = Solution::U32((314, 896));

struct Blizzard {
    pos: i32,
    dir: i32,
    reset: i32
}
impl Blizzard {
    fn new(pos: usize, dir: i32, reset: i32) -> Self {
        Self { pos: pos as i32, dir, reset }
    }
}

pub fn solve(input: String) -> Solution {
    let w = input.bytes().position(|c| c == b'\n').unwrap();
    let h = input.as_bytes().len() / (w + 1);
    let wi = w as i32;
    let hi = h as i32;
    
    let mut iter = input.bytes();
    let mut blizzards = Vec::with_capacity((w - 3) * (h - 3));
    let mut pos = 0;
    while let Some(c) = iter.next() {
        match c {
            b'.' | b'#' => pos += 1,
            b'\n' => {}
            b'>' => {
                blizzards.push(Blizzard::new(pos, 1, 3 - wi));
                pos += 1;
            }
            b'<' => {
                blizzards.push(Blizzard::new(pos, -1, wi - 3));
                pos += 1;
            }
            b'^' => {
                blizzards.push(Blizzard::new(pos, -wi, (hi - 3) * wi));
                pos += 1;
            }
            b'v' => {
                blizzards.push(Blizzard::new(pos, wi, (3 - hi) * wi));
                pos += 1;
            }
            other => panic!("unexpected char {}", other),
        }
    }
    
    let start = 1;
    let goal = w * h - 2;
    let mut empty_map = Vec::new();
    empty_map.resize(w, u8::MAX);
    empty_map.resize(w * h - w, 0);
    empty_map.resize(w * h, u8::MAX);
    for y in 1..h {
        empty_map[y * w - 1] = u8::MAX;
        empty_map[y * w] = u8::MAX;
    }
    empty_map[start] = 0;
    empty_map[goal] = 0;
    
    let mut period = (w - 2) * (h - 2);
    if h % 5 == 2 && w % 5 == 2 {
        period /= 5;
    }
    let mut maps = Vec::with_capacity(period);
    for _ in 0..period {
        let mut map = empty_map.clone();
        for bliz in blizzards.iter_mut() {
            map[bliz.pos as usize] = 1;
            if empty_map[(bliz.pos + bliz.dir) as usize] != u8::MAX {
                bliz.pos += bliz.dir;
            } else {
                bliz.pos += bliz.reset;
            }
        }
        maps.push(map.into_boxed_slice());
    }
    
    let mut history = HashSet::new();
    let mut queue = VecDeque::new();
    
    let there = find_path(&maps, w, &mut history, &mut queue, 0, start, goal - w);
    history.clear();
    queue.clear();
    let back = find_path(&maps, w, &mut history, &mut queue, there, goal, start + w);
    history.clear();
    queue.clear();
    let there_again = find_path(&maps, w, &mut history, &mut queue, back, start, goal - w);
    
    (there as u32, there_again as u32).into()
}

fn find_path(maps: &[Box<[u8]>], w: usize, history: &mut HashSet<(usize, usize)>, queue: &mut VecDeque<usize>, time: usize, start: usize, goal: usize) -> usize {
    let mut time = time + 1;
    let mut map = &maps[time % maps.len()];
    queue.push_back(start);
    queue.push_back(0);
    while let Some(pos) = queue.pop_front() {
        if pos != 0 {
            if pos == goal {
                return time;
            }
            if !history.insert((pos, time)) {
                continue;
            }
            if map[pos] == 0 {
                queue.push_back(pos);
            }
            if pos < map.len() - w && map[pos + w] == 0 {
                queue.push_back(pos + w);
            }
            if pos > w && map[pos - w] == 0 {
                queue.push_back(pos - w);
            }
            if map[pos + 1] == 0 {
                queue.push_back(pos + 1);
            }
            if map[pos - 1] == 0 {
                queue.push_back(pos - 1);
            }
        } else {
            if queue.is_empty() {
                panic!("help me, i'm stuck in blizzard");
            }
            queue.push_back(0);
            time += 1;
            map = &maps[time % maps.len()];
        }
    }
    unreachable!();
}
