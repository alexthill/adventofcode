use aoc_lib_rust::{Day, Example, Solution};

pub fn knot_hash(input: &[u8]) -> u128 {
    let mut list: [u8; 256] = std::array::from_fn(|i| i as u8);
    let len = list.len();
    let mut curr = len;
    let mut skip = 0;

    for _ in 0..64 {
        for length in input.iter().copied().chain([17, 31, 73, 47, 23]) {
            let mut l = curr;
            let mut r = l + length as usize - 1;
            while l < r {
                list.swap(l % len, r %len);
                l += 1;
                r -= 1;
            }
            curr += length as usize + skip;
            skip += 1;
        }
    }

    list.chunks(16)
        .map(|chunk| chunk.iter().copied().reduce(|acc, e| acc ^ e).unwrap() as u128)
        .reduce(|acc, n| (acc << 8) | n)
        .unwrap()
}

pub struct Day10;

impl Day for Day10 {

    const PART1: Solution = Solution::U32(8536);
    const PART2: Solution = Solution::Str("aff593797989d665349efe11bb4fd99b");

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let sol1 = {
            let lengths = input.split(',')
                .filter_map(|s| s.parse::<usize>().ok())
                .collect::<Vec<_>>();
            let mut list = (0..=if example { 4 } else { 255 }).collect::<Vec<u8>>();
            let len = list.len();
            let mut curr = len;

            for (skip, length) in lengths.into_iter().enumerate() {
                let mut l = curr;
                let mut r = l + length - 1;
                while l < r {
                    list.swap(l % len, r %len);
                    l += 1;
                    r -= 1;
                }
                curr += length + skip;
            }

            list[0] as u32 * list[1] as u32
        };
        let sol2 = format!("{:032x}", knot_hash(input.as_bytes()));

        [Solution::U32(sol1), Solution::String(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(12), Solution::None],
            input: "3,4,1,5",
        },
        Example {
            solution: [Solution::U32(0), Solution::Str("a2582a3a0e66e6e86e3812dcb672a272")],
            input: "",
        },
        Example {
            solution: [Solution::U32(0), Solution::Str("3efbe78a8d82f29979031a4aa0b16a9d")],
            input: "1,2,3",
        },
    ];
}
