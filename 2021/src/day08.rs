use std::fs;
use std::collections::HashMap;

const DIGITS: [&'static str; 10] = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"];

pub fn solve() {
    let input = fs::read_to_string("inputs/08.txt").unwrap();
    // let nums: Vec<i32> = input.trim().split(',').map(|num| num.parse::<i32>().unwrap()).collect();
    
    let mut len_to_digit: HashMap<usize, usize> = HashMap::new();
    len_to_digit.insert(2, 1);
    len_to_digit.insert(4, 4);
    len_to_digit.insert(3, 7);
    len_to_digit.insert(7, 8);
    
    let mut easy_count = 0;
    let mut sum: usize = 0;
    for line in input.lines() {
        let mut line = line.split(" | ");
        let signals: Vec<_> = line.next().unwrap().split(' ').collect();
        let values: Vec<_>  = line.next().unwrap().split(' ').collect();
        let mut poss = [[true; 7]; 7];
        
        for value in values.iter() {
            if solve_easy(&mut poss, value, &len_to_digit) {
                easy_count += 1;
            }
        }
        for value in signals.iter() {
            solve_easy(&mut poss, value, &len_to_digit);
        }
        
        loop {
            let mut only_one = None;
            for (i, row) in poss.iter().enumerate() {
                let still_poss: Vec<_> = row.iter().enumerate().filter_map(|(pos, b)| if *b { Some(pos) } else { None }).collect();
                if still_poss.len() == 1 {
                    only_one = Some((i, still_poss[0]));
                }
            }
            let mut some_change = false;
            if let Some((x, y)) = only_one {
                for (i, row) in poss.iter_mut().enumerate() {
                    if i != x && row[y] {
                        row[y] = false;
                        some_change = true;
                    }
                }
            }
            if !some_change {
                break;
            }
        }
        
        sum += backtracking(0, &poss, &signals, &values).unwrap()
            .iter()
            .take(4)
            .rev()
            .enumerate()
            .map(|(i, d)| d * 10usize.pow(i as u32))
            .sum::<usize>();
    }
    
    println!("easy count: {}", easy_count);
    println!("sum: {}", sum);
}

fn backtracking(char_num: usize, poss: &[[bool; 7]; 7], signals: &Vec<&str>, values: &Vec<&str>) -> Option<Vec<usize>> {
    if char_num == 7 {
        let map: Vec<_> = poss.iter().map(|row| (row.iter().position(|b| *b).unwrap() as u8 + b'a') as char).collect();
        let mut res = Vec::new();
        for value in values.iter().chain(signals.iter()) {
            let mut mapped: Vec<_> = value.chars().map(|ch| map[index(ch)]).collect();
            mapped.sort_unstable();
            if let Some(digit) = DIGITS.iter().position(|d| d.chars().eq(mapped.iter().copied())) {
                res.push(digit);
            } else {
                return None;
            }
        }
        Some(res)
    } else {
        let poss_chars: Vec<_> = poss[char_num].iter().enumerate().filter_map(|(i, b)| if *b { Some(i) } else { None }).collect();
        if poss_chars.len() > 1 {
            for i in poss_chars {
                let mut poss = poss.clone();
                for row in poss.iter_mut() {
                    row[i] = false;
                }
                for (j, p) in poss[char_num].iter_mut().enumerate() {
                    *p = if i == j { true } else { false };
                }
                let res = backtracking(char_num + 1, &poss, signals, values);
                if res.is_some() {
                    return res;
                }
            }
            None
        } else {
            backtracking(char_num + 1, poss, signals, values)
        }
    }
}

fn solve_easy(poss: &mut [[bool; 7]; 7], value: &str, len_to_digit: &HashMap<usize, usize>) -> bool {
    if let Some(digit) = len_to_digit.get(&value.len()) {
        for ch in 'a'..='g' {
            if value.contains(ch) {
                for (ch2, p) in ('a'..='g').zip(poss[index(ch)].iter_mut()) {
                    *p = *p && DIGITS[*digit].contains(ch2)
                }
            } else {
                for ch2 in DIGITS[*digit].chars() {
                   poss[index(ch)][index(ch2)] = false;
                }
            }
        }
        true
    } else {
        false
    }
}

fn index(ch: char) -> usize {
    ch as usize - 'a' as usize
}
