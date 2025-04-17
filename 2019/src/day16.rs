use aoc_lib_rust::{Day, Example, Solution};

pub struct Day16;

impl Day for Day16 {

    const PART1: Solution = Solution::U32(76795888);
    const PART2: Solution = Solution::U32(84024125);

    fn solve2(input: &str, example: bool) -> [Solution; 2] {
        let nums = input.bytes().map(|b| (b - b'0') as i32).collect::<Vec<_>>();

        fn fft(signal: &[i32], phases: u32, repeat: usize) -> Vec<i32> {
            let mut full_signal = signal.repeat(repeat);
            let offset = signal[..7].iter().fold(0, |acc, n| acc * 10 + n) as usize;
            assert!(offset > full_signal.len() / 2);

            let len = full_signal.len();
            let partial_signal = &mut full_signal[offset..len];

            for _ in 0..phases {
                let mut total = 0;
                for i in (0..partial_signal.len()).rev() {
                    total = (total + partial_signal[i]) % 10;
                    partial_signal[i] = total;
                }
            }
            partial_signal.to_vec()
        }

        let sol1 = {
            let mut nums = nums.clone();
            let mut next = nums.clone();
            for _ in 0..100 {
                #[allow(clippy::needless_range_loop)]
                for i in 0..nums.len() {
                    let mut sum = 0;
                    for (j, &num) in nums.iter().enumerate() {
                        sum += num * [0, 1, 0, -1][((j + 1) / (i + 1)) % 4];
                    }
                    next[i] = sum.abs() % 10;
                }
                std::mem::swap(&mut nums, &mut next);
            }
            nums.iter().take(8).fold(0, |acc, num| acc * 10 + *num)
        };

        let sol2 = if !example {
            let nums2 = fft(&nums, 100, 10_000);
            Solution::U32(nums2.iter().take(8).fold(0, |acc, num| acc * 10 + *num) as _)
        } else {
            Solution::None
        };

        [Solution::U32(sol1 as _), sol2]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(24176176), Solution::None],
            input: "80871224585914546619083218645595",
        },
        Example {
            solution: [Solution::U32(73745418), Solution::None],
            input: "19617804207202209144916044189917",
        },
        Example {
            solution: [Solution::U32(52432133), Solution::None],
            input: "69317163492948606335995924319873",
        },
    ];
}
