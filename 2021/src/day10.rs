use std::collections::HashMap;
use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/10.txt").unwrap();
    let mut map: HashMap<char, (char, usize)> = HashMap::new();
    map.insert(')', ('(', 3));
    map.insert(']', ('[', 57));
    map.insert('}', ('{', 1197));
    map.insert('>', ('<', 25137));
    
    let mut corrupted_score: usize = 0;
    let mut missing_scores = vec![];
    let mut stack = vec![];
    'outer: for line in input.lines() {
        for ch in line.chars() {
            match ch {
                ch @ ('(' | '[' | '{' | '<') => stack.push(ch),
                ch => {
                    let (opening, score) = *map.get(&ch).unwrap();
                    if stack.pop().unwrap() != opening {
                        stack.clear();
                        corrupted_score += score;
                        continue 'outer;
                    }
                }
            }
        }
        let mut missing_score: usize = 0;
        while let Some(ch) = stack.pop() {
            let score = match ch {
                '(' => 1,
                '[' => 2,
                '{' => 3,
                '<' => 4,
                _ => unreachable!(),
            };
            missing_score = missing_score * 5 + score;
        }
        missing_scores.push(missing_score);
    }
    missing_scores.sort_unstable();
    println!("corrupted score : {}", corrupted_score);
    println!("missing score: {}", missing_scores[missing_scores.len() / 2]);
}
