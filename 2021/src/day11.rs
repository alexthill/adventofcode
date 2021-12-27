use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/11.txt").unwrap();
    let mut nums: Vec<Vec<i32>> = input.lines().map(|line| line.chars().map(|ch| ch.to_digit(10).unwrap() as i32).collect()).collect();
    let dims = (nums[0].len(), nums.len());
    
    let mut flash_count: usize = 0;
    for step_num in 1.. {
        let mut flash = false;
        let mut step_flash_count: usize = 0;
        for row in nums.iter_mut() {
            for num in row.iter_mut() {
                *num += 1;
                if *num > 9 {
                    flash = true;
                }
            }
        }
        while flash {
            flash = false;
            for j in 0..dims.1 {
                for i in 0..dims.0 {
                    if nums[j][i] > 9 {
                        flash_count += 1;
                        step_flash_count += 1;
                        flash = true;
                        nums[j][i] = 0;
                        if i != 0 {
                            if nums[j][i-1] != 0 { nums[j][i-1] += 1; }
                            if j != 0 && nums[j-1][i-1] != 0 { nums[j-1][i-1] += 1; }
                            if j != dims.1-1 && nums[j+1][i-1] != 0 { nums[j+1][i-1] += 1; }
                        }
                        if i != dims.0-1 {
                            if nums[j][i+1] != 0 { nums[j][i+1] += 1; }
                            if j != 0 && nums[j-1][i+1] != 0 { nums[j-1][i+1] += 1; }
                            if j != dims.1-1 && nums[j+1][i+1] != 0 { nums[j+1][i+1] += 1; }
                        }
                        if j != 0 {
                            if nums[j-1][i] != 0 { nums[j-1][i] += 1; }
                        }
                        if j != dims.1-1 {
                            if nums[j+1][i] != 0 { nums[j+1][i] += 1; }
                        }
                    }
                }
            }
        }
        
        if step_num == 100 {
            println!("flash count after 100 steps: {}", flash_count);
        }
        if step_flash_count == dims.0 * dims.1 {
            println!("synchronized after {} steps", step_num);
            break;
        }
    }
    
}
