use crate::Solution;
use crate::utils::iter_parse_i32;

pub const SOLUTION: Solution = Solution::I32Str((10760, r#"
####.###...##..###..#..#.####..##..#..#.
#....#..#.#..#.#..#.#..#.#....#..#.#..#.
###..#..#.#....#..#.####.###..#....####.
#....###..#.##.###..#..#.#....#.##.#..#.
#....#....#..#.#....#..#.#....#..#.#..#.
#....#.....###.#....#..#.#.....###.#..#.
"#));

pub fn solve(input: String) -> Solution {
    let mut iter = input.into_bytes().into_iter();
    let mut sum = 0;
    let mut clock = 1;
    let mut reg = 1;
    let mut x: i32 = 0;
    let mut crt = Vec::with_capacity(41 * 6 + 1);
    crt.push(b'\n');
    
    while let Some(c) = iter.next() {
        sum += reg * clock * (clock % 40 == 20) as i32;
        crt.push(b'#' ^ (b'.' ^ b'#') * ((x - reg > 1) as u8 | (x - reg < -1) as u8));
        
        iter.nth(3).unwrap();
        if c == b'a' {
            clock += 1;
            x = (x + 1) % 40;
            if x == 0 { crt.push(b'\n'); }
            
            sum += reg * clock * (clock % 40 == 20) as i32;
            crt.push(b'#' ^ (b'.' ^ b'#') * ((x - reg > 1) as u8 | (x - reg < -1) as u8));
        
            reg += iter_parse_i32(&mut iter).0.unwrap();
        }
        
        clock += 1;
        x = (x + 1) % 40;
        if x == 0 { crt.push(b'\n'); }
    }
    
    (sum, String::from_utf8(crt).unwrap()).into()
}
