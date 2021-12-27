use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/01.txt").unwrap();
    let mut last_num = None;
    let mut increased_count = 0;
    for line in input.lines() {
        let num = line.parse::<u32>().unwrap();
        if let Some(last_num) = last_num {
            if num > last_num {
                increased_count += 1;
            }
        }
        last_num = Some(num);
    }
    println!("increasing measurement count: {}", increased_count);
    
    let mut lines = input.lines();
    let mut window = (
        lines.next().unwrap().parse::<u32>().unwrap(),
        lines.next().unwrap().parse::<u32>().unwrap(),
        lines.next().unwrap().parse::<u32>().unwrap()
    );
    let mut increased_count = 0;
    for line in lines {
        let old_sum = window.0 + window.1 + window.2;
        window = (window.1, window.2, line.parse::<u32>().unwrap());
        let new_sum = window.0 + window.1 + window.2;
        if new_sum > old_sum {
            increased_count += 1;
        }
    }
    println!("increasing window count: {}", increased_count);
}
