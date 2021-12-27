use std::fs;

#[derive(Debug, Clone)]
struct Range {
    coords: [(isize, isize); 3],
    is_on: bool,
    halves: Option<Box<[Range; 2]>>,
}
impl Range {
    fn new(coords: [(isize, isize); 3], is_on: bool) -> Self {
        assert!(coords.iter().all(|(start, end)| start <= end));
        Range { coords, is_on, halves: None }
    }
    
    fn volume(&self) -> isize {
        self.coords.iter().map(|(start, end)| end - start + 1).product::<isize>()
    }
    
    fn intersection(&self, other: &Range) -> Option<Range> {
        let mut coords = [(0, 0); 3];
        for i in 0..3 {
            coords[i] = if self.coords[i].0 <= other.coords[i].0 && self.coords[i].1 >= other.coords[i].1 {
                // other is included in self
                other.coords[i]
            } else if self.coords[i].0 >= other.coords[i].0 && self.coords[i].1 <= other.coords[i].1 {
                // self is included in other
                self.coords[i]
            } else if self.coords[i].0 <= other.coords[i].0 && self.coords[i].1 >= other.coords[i].0 {
                // intersection with self left of other
                (other.coords[i].0, self.coords[i].1)
            } else if self.coords[i].0 >= other.coords[i].0 && self.coords[i].0 <= other.coords[i].1 {
                // intersection with self right of other
                (self.coords[i].0, other.coords[i].1)
            } else {
                return None;
            }
        }
        Some(Range::new(coords, other.is_on))
    }
    
    fn switch_range(&mut self, other: &Range) {
        if let Some(intersection) = self.intersection(other) {
            if self.is_on == other.is_on && self.halves.is_none() {
                return;
            }
            if self.coords.iter().all(|(start, end)| start == end) {
                self.is_on = other.is_on;
                return;
            }
            if self.coords == other.coords {
                self.is_on = other.is_on;
                self.halves = None;
                return;
            }
            self.split(&intersection);
            for half in self.halves.as_mut().unwrap().iter_mut() {
                if let Some(half_intersection) = half.intersection(&intersection) {
                    half.switch_range(&half_intersection);
                }
            }
        }
    }
    
    fn split(&mut self, other: &Range) {
        if self.halves.is_none() {
            let (max_dim, (start, end)) = self.coords.iter().enumerate()
                .filter(|(dim, range)| **range != other.coords[*dim])
                .max_by_key(|(_, (start, end))| end - start + 1)
                .unwrap();
            let size = (end - start + 1) / 2;
            let mut left = self.coords.clone();
            left[max_dim].1 = left[max_dim].0 + size - 1;
            let mut right = self.coords.clone();
            right[max_dim].0 = right[max_dim].0 + size;
                
            self.halves = Some(Box::new([
                Range::new(left, self.is_on),
                Range::new(right, self.is_on),
            ]));
        }
    }
    
    fn count_on(&self) -> isize {
        if let Some(halves) = &self.halves {
            halves[0].count_on() + halves[1].count_on()
        } else if self.is_on {
            self.volume()
        } else {
            0
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/22.txt").unwrap();
    let mut lines = input.lines();
    let mut range = Range::new([(i32::MIN as isize, i32::MAX as isize); 3], false);
    let mut is_init = true;
    while let Some(line) = lines.next() {
        let mut parts = line.split(' ');
        let is_on = parts.next().unwrap() == "on";
        let mut parts = parts.next().unwrap().split(',');
        let mut coords = [(0, 0); 3];
        for i in 0..3 {
            let mut parts = parts.next().unwrap().split("..");
            let start = parts.next().unwrap()[2..].parse::<isize>().unwrap();
            let end = parts.next().unwrap().parse::<isize>().unwrap();
            coords[i] = (start, end);
        }
        
        if is_init && (coords[0].0 < -50 || coords[0].0 > 50) {
            println!("on count after init: {}", range.count_on());
            is_init = false;
        }
        range.switch_range(&Range::new(coords, is_on));
    }
    
    println!("on count after reboot: {}", range.count_on());
}
