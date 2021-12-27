use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/02.txt").unwrap();
    let mut depth = 0;
    let mut pos = 0;
    for line in input.lines() {
        let mut splitted = line.split(' ');
        let command = splitted.next().unwrap();
        let num = splitted.next().unwrap().parse::<u32>().unwrap();
        match command {
            "forward" => pos += num,
            "up" => depth -= num,
            "down" => depth += num,
            other => panic!("unexpected command '{}'", other),
        }
    }
    println!("pos: {}, depth: {}, pos * depth = {}", pos, depth, pos * depth);
    
    let mut depth = 0;
    let mut pos = 0;
    let mut aim = 0;
    for line in input.lines() {
        let mut splitted = line.split(' ');
        let command = splitted.next().unwrap();
        let num = splitted.next().unwrap().parse::<u32>().unwrap();
        match command {
            "forward" => {
                pos += num;
                depth += aim * num;
            }
            "up" => aim -= num,
            "down" => aim += num,
            other => panic!("unexpected command '{}'", other),
        }
    }
    println!("pos: {}, depth: {}, pos * depth = {}", pos, depth, pos * depth);
}
