use aoc_lib_rust::{Day, Example, Solution};

pub struct Day08;

impl Day for Day08 {

    const PART1: Solution = Solution::U32(1806);
    const PART2: Solution = Solution::Str("
  OO  OO  OOOO OOO   OO  
   O O  O O    O  O O  O 
   O O  O OOO  O  O O  O 
   O OOOO O    OOO  OOOO 
O  O O  O O    O O  O  O 
 OO  O  O O    O  O O  O 
");

    fn solve(input: &str) -> [Solution; 2] {
        const W: usize = 25;
        const H: usize = 6;

        let mut sol1 = 0;
        let mut min = u32::MAX;
        let mut curr = [0; 3];
        for (i, c) in input.bytes().enumerate() {
            curr[(c - b'0') as usize] += 1;
            if (i + 1) % (W * H) == 0 {
                if curr[0] < min {
                    min = curr[0];
                    sol1 = curr[1] * curr[2];
                }
                curr = [0; 3];
            }
        }

        let input = input.as_bytes();
        let mut img = [b'2'; W * H];
        for i in 0..W * H {
            let mut layer = 0;
            loop {
                let pixel = input[i + layer * W * H];
                if pixel != b'2' {
                    img[i] = pixel;
                    break;
                }
                layer += 1;
            }
        }
        let mut sol2 = "\n".to_owned();
        sol2.extend(img.chunks(W).map(|row| {
            let row = row.iter().map(|&c| {
                match c {
                    b'0' => b' ',
                    _ => b'O',
                }
            }).collect::<Vec<_>>();
            String::from_utf8(row).unwrap() + "\n"
        }));

        [Solution::U32(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[];
}
