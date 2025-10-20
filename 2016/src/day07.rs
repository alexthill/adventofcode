use aoc_lib_rust::{Day, Example, Solution};
use std::collections::HashSet;

pub struct Day07;

impl Day for Day07 {

    const PART1: Solution = Solution::U32(105);
    const PART2: Solution = Solution::U32(258);

    fn solve(input: &str) -> [Solution; 2] {
        let sol1 = input.lines().filter(|line| {
            let mut found = false;
            let mut in_brackets = false;
            for window in line.as_bytes().windows(4) {
                if window.contains(&b'[') {
                    in_brackets = true;
                }
                if window.contains(&b']') {
                    in_brackets = false;
                }
                if window[0] == window[3] && window[1] == window[2] && window[0] != window[1] {
                    if in_brackets {
                        return false;
                    }
                    found = true;
                }
            }
            found
        }).count();

        let sol2 = input.lines().filter(|line| {
            let mut abas = HashSet::new();
            let mut in_brackets = false;
            for window in line.as_bytes().windows(3) {
                if window.contains(&b'[') {
                    in_brackets = true;
                }
                if window.contains(&b']') {
                    in_brackets = false;
                }
                if window[0] == window[2] && window[0] != window[1] {
                    if abas.contains(&(!in_brackets, window[1], window[0])) {
                        return true;
                    }
                    abas.insert((in_brackets, window[0], window[1]));
                }
            }
            false
        }).count();

        [Solution::U32(sol1 as _), Solution::U32(sol2 as _)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: "abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn",
        },
        Example {
            solution: [Solution::U32(0), Solution::U32(3)],
            input: "aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb",
        },
    ];
}
