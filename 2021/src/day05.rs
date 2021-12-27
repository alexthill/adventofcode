use std::fs;
use std::collections::HashMap;

pub fn solve() {
    let input = fs::read_to_string("inputs/05.txt").unwrap();
    let mut map: HashMap<(u32, u32), usize> = HashMap::new();
    for line in input.lines() {
        let mut splitted = line.split(" -> ");
        let mut start_splitted = splitted.next().unwrap().split(',');
        let mut end_splitted = splitted.next().unwrap().split(',');
        let start = (start_splitted.next().unwrap().parse::<u32>().unwrap(), start_splitted.next().unwrap().parse::<u32>().unwrap());
        let end = (end_splitted.next().unwrap().parse::<u32>().unwrap(), end_splitted.next().unwrap().parse::<u32>().unwrap());
        // println!("{:?} -> {:?}", start, end);
        if start.0 == end.0 {
            let range = if start.1 <= end.1 { start.1 ..= end.1 } else { end.1 ..= start.1 };
            for y in range {
                *map.entry((start.0, y)).or_insert(0) += 1;
            }
        } else if start.1 == end.1 {
            let range = if start.0 <= end.0 { start.0 ..= end.0 } else { end.0 ..= start.0 };
            for x in range {
                *map.entry((x, start.1)).or_insert(0) += 1;
            }
        } else {
            let range_x: Box<dyn Iterator<Item=u32>> = if start.0 <= end.0 { Box::new(start.0 ..= end.0) } else { Box::new((end.0 ..= start.0).rev()) };
            let range_y: Box<dyn Iterator<Item=u32>> = if start.1 <= end.1 { Box::new(start.1 ..= end.1) } else { Box::new((end.1 ..= start.1).rev()) };
            for (x, y) in range_x.zip(range_y) {
                *map.entry((x, y)).or_insert(0) += 1;
            }
        }
    }
    let count: usize = map.iter().filter(|(_, val)| **val > 1).count();
    println!("dangerous count: {}", count);
}
