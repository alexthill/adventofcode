use aoc_lib_rust::{Day, Example, Solution};

pub struct Day02;

impl Day for Day02 {

    const PART1: Solution = Solution::U64(55916882972);
    const PART2: Solution = Solution::U64(76169125915);

    fn solve(input: &str) -> [Solution; 2] {
        let mut sol1 = 0;
        let mut sol2 = 0;
        for range in input.split(',') {
            let (start, end) = range.split_once('-').unwrap();
            let start = start.parse::<u64>().unwrap();
            let end = end.parse::<u64>().unwrap();

            for n in start..=end {
                let mut div = 10;
                while n * 10 >= div * div {
                    if n % div == n / div {
                        sol1 += n;
                        break;
                    }
                    div *= 10;
                }

                let mut div = 10;
                while n * 10 >= div * div {
                    let d = n % div;
                    let mut m = n / div;
                    while m != 0 && m % div == d {
                        m /= div;
                    }
                    if m == 0 && d * 10 >= div {
                        sol2 += n;
                        break;
                    }
                    div *= 10;
                }
            }
        }

        [Solution::U64(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U64(1227775554), Solution::U64(4174379265)],
            input: "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124",
        },
    ];
}
