pub fn solve(input: String) -> (u32, u32) {
    let mut items_group: u64 = !0;
    let mut i = 0;
    let mut sum1 = 0;
    let mut sum2 = 0;
    
    for line in input.as_bytes().split(|&c| c == b'\n') {
        let (a, b) = line.split_at(line.len() / 2);
        let mut items_a: u64 = 0;
        let mut items_b: u64 = 0;
        
        for (char_a, char_b) in a.iter().copied().zip(b.iter().copied()) {
            // convert 'a'..='z' to 1..=26 and 'A'..='Z' to 27..=52
            let pos_a = char_a - 38 - 58 * ((char_a & 32) >> 5);
            items_a |= 1 << pos_a;
            
            let pos_b = char_b - 38 - 58 * ((char_b & 32) >> 5);
            items_b |= 1 << pos_b;
        }
        
        sum1 += (items_a & items_b).trailing_zeros();
        items_group &= items_a | items_b;
        
        if i != 2 {
            i += 1;
        } else {
            sum2 += items_group.trailing_zeros();
            items_group = !0;
            i = 0;
        }
    }
    (sum1, sum2)
}
