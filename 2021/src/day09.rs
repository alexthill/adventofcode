use std::fs;

pub fn solve() {
    let input = fs::read_to_string("inputs/09.txt").unwrap();
    let mut nums: Vec<Vec<i32>> = input.lines().map(|line| line.chars().map(|ch| ch.to_digit(10).unwrap() as i32).collect()).collect();
    let dims = (nums[0].len(), nums.len());
    
    let mut sum = 0;
    let mut sizes = vec![];
    for j in 0..dims.1 {
        for i in 0..dims.0 {
            let val = nums[j][i];
            if i != 0 && nums[j][i - 1] <= val { continue; }
            if i != dims.0 - 1 && nums[j][i + 1] <= val { continue; }
            if j != 0 && nums[j - 1][i] <= val { continue; }
            if j != dims.1 - 1 && nums[j + 1][i] <= val { continue; }
            sum += 1 + val;
            
            let mut size = 0;
            let mut queue = vec![(i, j)];
            while let Some((i, j)) = queue.pop() {
                let val = nums[j][i];
                if val == -1 { continue; }
                nums[j][i] = -1;
                size += 1;
                if i != 0 && nums[j][i - 1] > val && nums[j][i - 1] != 9 { queue.push((i - 1, j)); }
                if i != dims.0 - 1 && nums[j][i + 1] > val && nums[j][i + 1] != 9 { queue.push((i + 1, j)); }
                if j != 0 && nums[j - 1][i] > val && nums[j - 1][i] != 9 { queue.push((i, j - 1)); }
                if j != dims.1 - 1 && nums[j + 1][i] > val && nums[j + 1][i] != 9 { queue.push((i, j + 1)); }
            }
            sizes.push(size);
        }
    }
    
    println!("risk sum: {}", sum);
    sizes.sort_unstable();
    let mut iter = sizes.iter().rev();
    println!("size product: {:?}", iter.next().unwrap() * iter.next().unwrap() * iter.next().unwrap());
}
