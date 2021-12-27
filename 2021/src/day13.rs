use std::collections::HashSet;
use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/13.txt").unwrap();
    let mut dots: HashSet<(i32, i32)> = HashSet::new();
    let mut folds = vec![];
    let mut first_part = true;
    for line in input.lines() {
        if first_part {
            if line.is_empty() {
                first_part = false;
            } else {
                let mut splitted = line.split(',');
                let x = splitted.next().unwrap().parse::<i32>().unwrap();
                let y = splitted.next().unwrap().parse::<i32>().unwrap();
                dots.insert((x, y));
            }
        } else {
            let is_x_fold = line.contains('x');
            let coord = line.split('=').nth(1).unwrap().parse::<i32>().unwrap();
            folds.push((is_x_fold, coord));
        }
    }
    
    for (i, &(is_x_fold, coord)) in folds.iter().enumerate() {
        let mut new_dots = HashSet::new();
        for mut dot in dots.drain() {
            if is_x_fold {
                let diff = dot.0 - coord;
                if diff > 0 {
                    dot.0 = coord - diff;
                }
            } else {
                let diff = dot.1 - coord;
                if diff > 0 {
                    dot.1 = coord - diff;
                }
            }
            new_dots.insert(dot);
        }
        
        dots = new_dots;
        
        if i == 0 {
            println!("dot count after first fold: {}", dots.len());
        }
    }
    
    let mut paper = vec![];
    for _ in 0..6 {
        let mut row = vec![];
        row.resize(40, '.');
        paper.push(row);
    }
    
    for dot in dots {
        paper[dot.1 as usize][dot.0 as usize] = '#';
    }
    
    println!("Code: ");
    println!("{}", paper.iter().map(|row| row.iter().collect::<String>()).collect::<Vec<String>>().join("\n"));
}
