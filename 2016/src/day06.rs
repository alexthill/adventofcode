use aoc_lib_rust::{Day, Example, Solution};

pub struct Day06;

impl Day for Day06 {

    const PART1: Solution = Solution::Str("qzedlxso");
    const PART2: Solution = Solution::Str("ucmifjae");

    fn solve(input: &str) -> [Solution; 2] {
        let mut freq = [[0; 26]; 16];
        for line in input.lines() {
            for (i, ch) in line.bytes().enumerate() {
                freq[i][(ch - b'a') as usize] += 1;
            }
        }

        let mut sol1 = String::new();
        let mut sol2 = String::new();
        for f in freq {
            if let Some((i, _)) = f.into_iter().enumerate()
                .filter(|(_, n)| *n != 0)
                .max_by_key(|(_, n)| *n)
            {
                sol1.push(char::from_u32((i as u8 + b'a') as _).unwrap());
            }
            if let Some((i, _)) = f.into_iter().enumerate()
                .filter(|(_, n)| *n != 0)
                .min_by_key(|(_, n)| *n)
            {
                sol2.push(char::from_u32((i as u8 + b'a') as _).unwrap());
            }
        }

        [Solution::String(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::Str("easter"), Solution::Str("advent")],
            input: "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar",
        },
    ];
}
