pub fn solve() {
    let hall = [0; 11];
    
    let rooms = [[10, 100], [10, 1], [1000, 1], [1000, 100]];
    print_map(&rooms, &hall);
    println!("min cost: {}", find(&rooms, &hall));

    let rooms = [[10, 1000, 1000, 100], [10, 100, 10, 1], [1000, 10, 1, 1], [1000, 1, 100, 100]];
    print_map(&rooms, &hall);
    println!("min cost: {}", find(&rooms, &hall));
}

fn find<const N: usize>(rooms: &[[i32; N]; 4], hall: &[i32; 11]) -> i32 {
    let mut min_cost = i32::MAX / 2;
    let mut fixed_cost = 0;
    let mut hall = hall.clone();
    let mut rooms = rooms.clone();
    
    let mut moved = true;
    while moved {
        moved = false;
        'hall_loop: for i in 0..hall.len() {
            let val = hall[i];
            if val == 0 {
                continue;
            }
            let room_id = val_to_room(val);
            let room_num = room_id * 2 + 2;
            let hall_cost = if i < room_num {
                for j in i+1..room_num {
                    if hall[j] != 0 {
                        continue 'hall_loop;
                    }
                }
                room_num - i
            } else {
                for j in room_num+1..i {
                    if hall[j] != 0 {
                        continue 'hall_loop;
                    }
                }
                i - room_num
            };
            if rooms[room_id][0] != 0 {
                continue;
            }
            let room_pos = rooms[room_id].iter().position(|val| *val != 0).unwrap_or(rooms[room_id].len()) - 1;
            if rooms[room_id][room_pos+1..].iter().any(|val| val_to_room(*val) != room_id) {
                continue;
            }
            hall[i] = 0;
            rooms[room_id][room_pos] = val;
            moved = true;
            fixed_cost += (hall_cost + room_pos + 1) as i32 * val;
        }
    }
    
    if fixed_cost > 0 && hall.iter().all(|val| *val == 0) {
        return fixed_cost;
    }
    
    for room_id in 0..rooms.len() {
        let room_pos = if let Some(first_occupied) = rooms[room_id].iter().position(|val| *val != 0) {
            if rooms[room_id][first_occupied..].iter().all(|val| val_to_room(*val) == room_id) {
                continue;
            }
            first_occupied
        } else {
            continue;
        };
        let room_num = room_id * 2 + 2;
        let val = rooms[room_id][room_pos];
        rooms[room_id][room_pos] = 0;
        for i in (0..room_num).rev() {
            if i % 2 == 0 && i != 0 {
                continue;
            }
            if hall[i] != 0 {
                break;
            }
            hall[i] = val;
            min_cost = min_cost.min(find(&rooms, &hall) + (room_pos + 1 + room_num - i) as i32 * val);
            hall[i] = 0;
        }
        for i in room_num+1..hall.len() {
            if i % 2 == 0 && i != hall.len() - 1 {
                continue;
            }
            if hall[i] != 0 {
                break;
            }
            hall[i] = val;
            min_cost = min_cost.min(find(&rooms, &hall) + (room_pos + 1 + i - room_num) as i32 * val);
            hall[i] = 0;
        }
        rooms[room_id][room_pos] = val;
    }
    min_cost + fixed_cost
}

fn val_to_room(val: i32) -> usize {
    match val {
        1 => 0,
        10 => 1,
        100 => 2,
        1000 => 3,
        _ => usize::MAX,
    }
}

fn val_to_char(val: i32) -> char {
    match val {
        0 => '.',
        1 => 'A',
        10 => 'B',
        100 => 'C',
        1000 => 'D',
        _ => unreachable!(),
    }
}

fn print_map<const N: usize>(rooms: &[[i32; N]; 4], hall: &[i32; 11]) {
    println!("{}", hall.iter().map(|val| val_to_char(*val)).collect::<String>());
    for i in 0..N {
        println!("  {}", rooms.iter().map(|room| [val_to_char(room[i]), ' '].iter().collect::<String>()).collect::<String>());
    }
    
}
