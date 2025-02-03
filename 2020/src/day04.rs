use aoc_lib_rust::{Day, Example, Solution};

pub struct Day04;

impl Day for Day04 {

    const PART1: Solution = Solution::U32(213);
    const PART2: Solution = Solution::U32(147);

    fn solve(input: &str) -> [Solution; 2] {
        fn is_num_between(s: &str, min: u16, max: u16) -> bool {
            s.parse::<u16>().map(|num| num >= min && num <= max).unwrap_or(false)
        }

        fn validate_field(field: &str, value: &str) -> (i8, bool) {
            match field {
                "byr" => (0, is_num_between(value, 1920, 2002)),
                "iyr" => (1, is_num_between(value, 2010, 2020)),
                "eyr" => (2, is_num_between(value, 2020, 2030)),
                "hgt" => {
                    if value.len() == 5 && &value[3..] == "cm" {
                        (3, is_num_between(&value[..3], 150, 193))
                    } else if value.len() == 4 && &value[2..] == "in" {
                        (3, is_num_between(&value[..2], 59, 76))
                    } else {
                        (3, false)
                    }
                }
                "hcl" => {
                    let mut cs = value.chars();
                    let valid = value.len() == 7
                        && cs.next().map(|c| c =='#').unwrap_or(false)
                        && cs.all(|c| (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'));
                    (4, valid)
                }
                "ecl" => {
                    match value {
                        "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => (5, true),
                        _ => (5, false),
                    }
                }
                "pid" => {
                    (6, value.len() == 9 && value.chars().all(|c| c.is_ascii_digit()))
                }
                _ => (-1, false),
            }
        }

        let mut sol1 = 0;
        let mut passport1 = 0_u8;
        let mut sol2 = 0;
        let mut passport2 = 0_u8;
        for line in input.split('\n') {
            if line.is_empty() {
                sol1 += (passport1 == 0b1111111) as u32;
                sol2 += (passport2 == 0b1111111) as u32;
                passport1 = 0;
                passport2 = 0;
                continue;
            }
            for entry in line.split(' ') {
                let mut parts = entry.split(':');
                let key = parts.next().unwrap();
                let value = parts.next().unwrap();
                let (num, valid) = validate_field(key, value);
                if num >= 0 {
                    passport1 |= 1 << num as u32;
                    passport2 |= (valid as u8) << num as u32;
                }
            }
        }
        sol1 += (passport1 == 0b1111111) as u32;
        sol2 += (passport2 == 0b1111111) as u32;

        [Solution::U32(sol1), Solution::U32(sol2)]
    }

    const EXAMPLES: &'static [Example] = &[
        Example {
            solution: [Solution::U32(2), Solution::None],
            input: "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in",
        },
        Example {
            solution: [Solution::None, Solution::U32(0)],
            input: "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007",
        },
        Example {
            solution: [Solution::None, Solution::U32(4)],
            input: "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719",
        },
    ];
}


