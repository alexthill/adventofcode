use crate::utils::iter_parse_u32;

pub fn solve(input: String) -> (u32, u32) {
    let mut sum1: u32 = 0;
    let mut sum2: u32 = 0;
    let mut iter = input.bytes();
    
    while let Some(a1) = iter_parse_u32(&mut iter).0 {
        let a2 = iter_parse_u32(&mut iter).0.unwrap();
        let b1 = iter_parse_u32(&mut iter).0.unwrap();
        let b2 = iter_parse_u32(&mut iter).0.unwrap();
        
        // this is faster than using ifs becuase branch prediction does not like randomized input
        sum1 += ((a1 >= b1) as u32 & (a2 <= b2) as u32) | ((b1 >= a1) as u32 & (b2 <= a2) as u32);
        sum2 += ((a1 <= b2) as u32 & (a2 >= b2) as u32) | ((b1 <= a2) as u32 & (b2 >= a2) as u32);
    }
    
    (sum1, sum2)
}
