use crate::Solution;

pub const SOLUTION: Solution = Solution::U32((7568, 2780));

pub fn solve(input: String) -> Solution {
    let mut items_group: u64 = !0;
    let mut i = 0;
    let mut sum1 = 0;
    let mut sum2 = 0;
    
    for line in input.trim_end().as_bytes().split(|&c| c == b'\n') {
        let (a, b) = line.split_at(line.len() / 2);
        let mut items_a: u64 = 0;
        let mut items_b: u64 = 0;
        
        for (&char_a, &char_b) in a.iter().zip(b.iter()) {
            // this maps 'A' == 65 to bit 1 and 'a' == 97 to bit 33
            items_a |= 1_u64.overflowing_shl(char_a as u32).0;
            items_b |= 1_u64.overflowing_shl(char_b as u32).0;
        }
        
        sum1 += items_to_score(items_a & items_b);
        items_group &= items_a | items_b;
        
        if i != 2 {
            i += 1;
        } else {
            sum2 += items_to_score(items_group);
            items_group = !0;
            i = 0;
        }
    }
    (sum1, sum2).into()
}

fn items_to_score(items: u64) -> u32 {
    // convert 'a'..='z' to 1..=26 and 'A'..='Z' to 27..=52
    // 'a' is bit 33 'A' is bit 1
    let pos = items.trailing_zeros();
    if pos < 32 {
        pos + 26
    } else {
        pos - 32
    }
}
