use aoc_lib_rust::{Day, Example, Solution};

pub struct Day19;

#[derive(Debug, Clone)]
enum Rule {
    Seq(Vec<usize>),
    Or(Vec<usize>, Vec<usize>),
    Terminal(u8),
}

impl Rule {
    fn matches(path: &mut Vec<usize>, rules: &[Rule], string: &[u8]) -> bool {
        if string.is_empty() || path.is_empty() {
            return string.is_empty() && path.is_empty();
        }
        let rule_idx = path.pop().unwrap();
        let len = path.len();
        let res = match &rules[rule_idx] {
            Rule::Seq(seq) => {
                path.extend(seq.iter().rev().copied());
                Rule::matches(path, rules, string)
            }
            Rule::Or(seq1, seq2) => {
                path.extend(seq1.iter().rev().copied());
                if Rule::matches(path, rules, string) {
                    true
                } else {
                    path.truncate(len);
                    path.extend(seq2.iter().rev().copied());
                    Rule::matches(path, rules, string)
                }
            }
            Rule::Terminal(terminal) if string[0] != *terminal => false,
            Rule::Terminal(_) => Rule::matches(path, rules, &string[1..]),
        };
        path.truncate(len);
        path.push(rule_idx);
        res
    }
}

impl Day for Day19 {

    const PART1: Solution = Solution::U32(269);
    const PART2: Solution = Solution::U32(403);

    fn solve(input: &str) -> [Solution; 2] {
        let mut rules = Vec::new();
        let mut lines = input.as_bytes().split(|c| *c == b'\n');
        for line in &mut lines {
            if line.is_empty() {
                break;
            }
            let parse_num = |acc, c| acc * 10 + (c - b'0') as usize;
            let mut parts = line.split(|c| *c == b' ');
            let id = parts.next().unwrap();
            let id = id[..id.len()-1].iter().fold(0, parse_num);

            let mut seq1 = Vec::new();
            let rule = loop {
                let Some(part) = parts.next() else {
                    break Rule::Seq(seq1);
                };
                if part[0] == b'"' {
                    break Rule::Terminal(part[1]);
                }
                if part == b"|" {
                    let seq2 = parts.map(|part| part.iter().fold(0, parse_num)).collect();
                    break Rule::Or(seq1, seq2);
                }
                seq1.push(part.iter().fold(0, parse_num));
            };
            if id >= rules.len() {
                rules.resize(id + 1, Rule::Terminal(b'_'));
            }
            rules[id] = rule;
        }

        let Rule::Seq(mut path) = rules[0].clone() else {
            unreachable!("rule 0 must be a simple sequence");
        };
        path.reverse();

        let sol1 = lines.clone().filter(|line| Rule::matches(&mut path, &rules, line)).count();
        let sol2 = if rules.len() >= 11 {
            rules[8] = Rule::Or(vec![42], vec![42, 8]);
            rules[11] = Rule::Or(vec![42, 31], vec![42, 11, 31]);
            lines.filter(|line| Rule::matches(&mut path, &rules, line)).count().into()
        } else {
            Solution::None
        };

        [Solution::U32(sol1 as _), sol2]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: r#"0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b"

aab
aba"#,
        },
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: r#"0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"#,
        },
        Example {
            solution: [Solution::U32(3), Solution::U32(12)],
            input: r#"42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"#,
        },
    ];
}
