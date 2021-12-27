use std::collections::HashMap;
use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/14.txt").unwrap();
    //let mut nums: Vec<Vec<i32>> = input.lines().map(|line| line.chars().map(|ch| ch.to_digit(10).unwrap() as i32).collect()).collect();
    //let dims = (nums[0].len(), nums.len());
    let mut lines = input.lines();
    let tmpl = lines.next().unwrap().chars().collect::<Vec<char>>();
    
    lines.next().unwrap();
    let mut rules = vec![];
    for line in lines {
        let mut splitted = line.split(" -> ");
        let mut first = splitted.next().unwrap().chars();
        rules.push(((first.next().unwrap(), first.next().unwrap()), splitted.next().unwrap().chars().next().unwrap()));
    }
    
    let mut pair_counts = HashMap::<(char, char), usize>::new();
    for pair in tmpl.iter().copied().zip(tmpl.iter().skip(1).copied()) {
        let count = pair_counts.entry(pair).or_insert(0);
        *count += 1;
    }
    for i in 1..=40 {
        let mut new_counts = HashMap::<(char, char), usize>::new();
        for rule in rules.iter() {
            if let Some(count) = pair_counts.get(&rule.0) {
                let new_count = new_counts.entry((rule.0.0, rule.1)).or_insert(0);
                *new_count += *count;
                let new_count = new_counts.entry((rule.1, rule.0.1)).or_insert(0);
                *new_count += *count;
            }
        }
        pair_counts = new_counts;
        
        if i == 10 {
            show_diff(10, &pair_counts);
        }
    }
    show_diff(40, &pair_counts);
    
}

fn show_diff(iter_count: usize, pair_counts: &HashMap<(char, char), usize>) {
    let mut counts = HashMap::<char, usize>::new();
    for (pair, pair_count) in pair_counts.iter() {
        let count = counts.entry(pair.0).or_insert(0);
        *count += pair_count;
        let count = counts.entry(pair.1).or_insert(0);
        *count += pair_count;
    }
    let mut counts = counts.into_iter().map(|(_, count)| count).collect::<Vec<_>>();
    counts.sort_unstable();
    let least = counts[0];
    let least = if least % 2 == 0 { least / 2 } else { least / 2 + 1 };
    let most = *counts.last().unwrap();
    let most = if most % 2 == 0 { most / 2 } else { most / 2 + 1 };
    println!("after {} iterations: {} - {} = {}", iter_count, most, least, most - least);
}
