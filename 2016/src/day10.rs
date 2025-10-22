use aoc_lib_rust::{next, next_parse, Day, Solution};

#[derive(Debug, Default, Clone, Copy)]
struct Bot {
    nums: [usize; 2],
    outs: [bool; 2],
    values: [Option<u32>; 2],
}

pub struct Day10;

impl Day for Day10 {

    const PART1: Solution = Solution::U32(147);
    const PART2: Solution = Solution::U32(55637);

    fn solve(input: &str) -> [Solution; 2] {
        let mut bots = Vec::new();
        for line in input.lines() {
            if line.as_bytes()[0] == b'v' {
                continue;
            }
            let mut parts = line.split(' ');
            assert_eq!(next!(parts), "bot");
            let bot = next_parse!(parts, usize);
            assert_eq!(next!(parts), "gives");
            assert_eq!(next!(parts), "low");
            assert_eq!(next!(parts), "to");
            let low_out = next!(parts) == "output";
            let low_num = next_parse!(parts, usize);
            assert_eq!(next!(parts), "and");
            assert_eq!(next!(parts), "high");
            assert_eq!(next!(parts), "to");
            let high_out = next!(parts) == "output";
            let high_num = next_parse!(parts, usize);
            if bots.len() <= bot {
                bots.resize(bot + 1, Bot::default());
            }
            bots[bot] = Bot {
                nums: [low_num, high_num],
                outs: [low_out, high_out],
                values: [None, None],
            };
        }

        fn hand_num(idx: usize, bots: &mut [Bot], outs: &mut Vec<u32>, value: u32) {
            if let Some(first_value) = bots[idx].values[0] {
                bots[idx].values[1] = Some(value);
                let values = if first_value < value {
                    [first_value, value]
                } else if first_value > value {
                    [value, first_value]
                } else {
                    panic!("values are equal: {value}");
                };

                for (i, value) in values.into_iter().enumerate() {
                    if bots[idx].outs[i] {
                        if outs.len() <= bots[idx].nums[i] {
                            outs.resize(bots[idx].nums[i] + 1, 0);
                        }
                        outs[bots[idx].nums[i]] = value;
                    } else {
                        hand_num(bots[idx].nums[i], bots, outs, value);
                    }
                }
            } else {
                bots[idx].values[0] = Some(value);
            }
        }

        let mut outs = Vec::new();
        for line in input.lines() {
            if line.as_bytes()[0] == b'b' {
                continue;
            }
            let mut parts = line.split(' ');
            assert_eq!(next!(parts), "value");
            let value = next_parse!(parts, u32);
            assert_eq!(next!(parts), "goes");
            assert_eq!(next!(parts), "to");
            assert_eq!(next!(parts), "bot");
            let bot = next_parse!(parts, usize);
            hand_num(bot, &mut bots, &mut outs, value);
        }

        let mut sol1 = 0;
        for (i, bot) in bots.iter().enumerate() {
            if bot.values == [Some(61), Some(17)] || bot.values == [Some(17), Some(61)] {
                sol1 = i;
                break;
            }
        }

        let sol2 = outs[0] * outs[1] * outs[2];

        [Solution::U32(sol1 as _), Solution::U32(sol2)]
    }
}
