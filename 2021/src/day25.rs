use std::fs;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Field {
    South,
    East,
    Empty,
}
impl Field {
    fn from_char(ch: char) -> Self {
        match ch {
            '>' => Self::East,
            'v' => Self::South,
            '.' => Self::Empty,
            other => panic!("bad char {:?}", other),
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/25.txt").unwrap();
    let mut map: Vec<Vec<Field>> = input.lines().map(|line| line.chars().map(|ch| Field::from_char(ch)).collect()).collect();
    let dims = (map[0].len(), map.len());
    
    for step in 1.. {
        let mut moved = false;
        let mut next_map = map.clone();
        for j in 0..dims.1 {
            for i in 0..dims.0 {
                if map[j][i] == Field::East && map[j][(i + 1) % dims.0] == Field::Empty {
                    next_map[j][i] = Field::Empty;
                    next_map[j][(i + 1) % dims.0] = Field::East;
                    moved = true;
                }
            }
        }
        map = next_map;
        let mut next_map = map.clone();
        for j in 0..dims.1 {
            for i in 0..dims.0 {
                if map[j][i] == Field::South && map[(j + 1) % dims.1][i] == Field::Empty {
                    next_map[j][i] = Field::Empty;
                    next_map[(j + 1) % dims.1][i] = Field::South;
                    moved = true;
                }
            }
        }
        if !moved {
            println!("stopped moving at step {}", step);
            break;
        }
        map = next_map;
    }
}
