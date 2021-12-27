use std:: fs;

pub fn solve() {
    const BOARD_SIZE: usize = 5;
    const MARKED: i32 = -1;
    const FIND_LOSING: bool = true;
    let input = fs::read_to_string("inputs/04.txt").unwrap();
    let mut lines = input.lines();
    let num_iter = lines.next().unwrap().split(',').map(|num| num.parse::<i32>().unwrap());
    let mut boards: Vec<(Vec<Vec<i32>>, bool)> = vec![];
    
    while let Some(_) = lines.next() {
        let mut board = vec![];
        for _ in 0..BOARD_SIZE {
            let row = lines.next().unwrap()
                .split_ascii_whitespace()
                .map(|num| num.parse::<i32>().unwrap())
                .collect();
            board.push(row);
        }
        boards.push((board, false));
    }
    
    for num in num_iter {
        for (board_num, (board, is_bingo)) in boards.iter_mut().enumerate() {
            if *is_bingo {
                continue;
            }
            
            for row in board.iter_mut() {
                for cell in row.iter_mut() {
                    if *cell == num {
                        *cell = MARKED;
                    }
                }
            }
            
            for row in board.iter() {
                let mut bingo = true;
                for cell in row.iter() {
                    if *cell != MARKED {
                        bingo = false;
                        break;
                    }
                }
                if bingo {
                    *is_bingo = true;
                    println!("found bingo at board {}", board_num);
                    bingo_fn::<MARKED>(board, num);
                    if !FIND_LOSING {
                        return;
                    }
                }
            }
            for i in 0..BOARD_SIZE {
                let mut bingo = true;
                for row in board.iter() {
                    if row[i] != MARKED {
                        bingo = false;
                        break;
                    }
                }
                if bingo {
                    *is_bingo = true;
                    println!("found bingo at board {}", board_num);
                    bingo_fn::<MARKED>(board, num);
                    if !FIND_LOSING {
                        return;
                    }
                }
            }
        }
    }
}
fn bingo_fn<const MARKED: i32>(board: &Vec<Vec<i32>>, num: i32) {
    let mut sum = 0;
    for row in board.iter() {
        for cell in row.iter() {
            if *cell != MARKED {
                sum += *cell;
            }
        }
    }
    println!("unmarked sum: {}, num: {}, sum * num = {}", sum, num, sum * num);
}
