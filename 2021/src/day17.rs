pub fn solve() {
    let target_area = (137..=171, -98..=-73);
    let acc = (-1, -1);
    let mut max_y: i32 = 0;
    let mut count = 0;
    for vx in 1..172 {
        for vy in -100..200 {
            let mut v = (vx, vy);
            let mut pos = (0, 0);
            let mut high_point = 0;
            for _t in 1..300 {
                pos.0 += v.0;
                pos.1 += v.1;
                if pos.1 > high_point {
                    high_point = pos.1;
                }
                if target_area.0.contains(&pos.0) && target_area.1.contains(&pos.1) {
                    if high_point > max_y {
                        max_y = high_point;
                    }
                    count += 1;
                    // println!("found start velo {},{} after t = {}", vx, vy, t);
                    break;
                }
                v.0 = 0.max(v.0 + acc.0);
                v.1 += acc.1;
            }
        }
    }
    
    println!("max y pos: {}", max_y);
    println!("count: {}", count);
}
