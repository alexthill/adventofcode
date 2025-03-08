use aoc_lib_rust::{Day, Example, Solution};

pub struct Day23;

impl Day for Day23 {

    const PART1: Solution = Solution::U32(94238657);
    const PART2: Solution = Solution::U64(3072905352);

    fn solve(input: &str) -> [Solution; 2] {
        assert_eq!(input.as_bytes().len(), 9);

        let mut nums = [0; 9];
        for (i, c) in input.as_bytes().iter().enumerate() {
            nums[i] = (*c - b'0') as u32 - 1;
        }

        let mut cups1 = [0; 9];
        for i in 0..8 {
            cups1[nums[i] as usize] = nums[i + 1];
        }
        cups1[nums[8] as usize] = nums[0];

        let mut cups2 = cups1.to_vec();
        cups2.reserve(1_000_000);
        cups2.extend(10..1_000_000);
        cups2.push(nums[0]);
        cups2[nums[8] as usize] = 9;
        debug_assert_eq!(cups2.len(), 1_000_000);

        fn do_round(curr: u32, cups: &mut[u32]) -> u32 {
            let picked1 = cups[curr as usize];
            let picked2 = cups[picked1 as usize];
            let picked3 = cups[picked2 as usize];
            let mut dest = curr;
            loop {
                if dest == 0 {
                    dest = cups.len() as _;
                }
                dest -= 1;
                if (dest != picked1) & (dest != picked2) & (dest != picked3) {
                    break;
                }
            }
            cups[curr as usize] = cups[picked3 as usize];
            cups[picked3 as usize] = cups[dest as usize];
            cups[dest as usize] = picked1;
            cups[curr as usize]
        }

        let mut curr = nums[0];
        for _ in 0..100 {
            curr = do_round(curr, &mut cups1);
        }
        let mut sol1 = 0;
        let mut curr = cups1[0];
        while curr != 0 {
            sol1 = sol1 * 10 + curr + 1;
            curr = cups1[curr as usize];
        }

        let mut curr = nums[0];
        for _ in 0..10_000_000 {
            curr = do_round(curr, &mut cups2);
        }
        let picked1 = cups2[0];
        let picked2 = cups2[picked1 as usize];
        let sol2 = (picked1 + 1) as u64 * (picked2 + 1) as u64;

        [Solution::U32(sol1), Solution::U64(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(67384529), Solution::U64(149245887792)],
            input: "389125467",
        },
    ];
}
