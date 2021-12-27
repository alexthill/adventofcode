use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/07.txt").unwrap();
    let nums: Vec<i32> = input.trim().split(',').map(|num| num.parse::<i32>().unwrap()).collect();
    
    let min = (1..2000).map(|avg| nums.iter().map(|num| (num - avg).abs()).sum::<i32>()).min();
    println!("fuel spent: {}", min.unwrap());
    
    let min = (1..2000).map(|avg| {
        nums.iter().map(|num| {
            let diff = (num - avg).abs();
            (diff * diff + diff) / 2
        }).sum::<i32>()
    }).min();
    println!("fuel spent: {}", min.unwrap());
}
