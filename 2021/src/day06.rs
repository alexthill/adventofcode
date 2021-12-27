use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/06.txt").unwrap();
    let mut fishes: Vec<u32> = input.split(',').map(|num| num.parse::<u32>().unwrap()).collect();
    let copy = fishes.clone();
    let mut new: Vec<u32> = vec![];
    for _ in 0..80 {
        for fish in fishes.iter_mut() {
            if *fish == 0 {
                *fish = 6;
                new.push(8);
            } else {
                *fish -= 1;
            }
        }
        fishes.append(&mut new);
    }
    println!("fishes after 80 days: {}", fishes.len());
    
    let mut gens: [usize; 9] = [0; 9];
    for fish in copy {
        gens[fish as usize] += 1;
    }
    for _ in 0..256 {
        gens.rotate_left(1);
        gens[6] += gens[8];
    }
    println!("fishes after 256 day: {}", gens.iter().sum::<usize>());
}
