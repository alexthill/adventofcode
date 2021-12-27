use std::fs;
use std::str;

pub fn solve() {
    let input = fs::read_to_string("inputs/03.txt").unwrap();
    let input: Vec<&[u8]> = input.lines().map(|line| line.as_bytes()).collect();
    let bits_set = get_bit_counts(&input);
    
    let mut gamma_str = "".to_string();
    let mut epsilon_str = "".to_string();
    let mut oxy_values = input.clone();
    let mut co2_values = input.clone();
    for (i, bit) in bits_set.into_iter().enumerate() {
        let (most, least) = if bit as usize * 2 >= input.len() {
            ('1', '0')
        } else {
            ('0', '1')
        };
        gamma_str += &String::from(most);
        epsilon_str += &String::from(least);
        if oxy_values.len() > 1 {
            let bits_set = get_bit_counts(&oxy_values);
            let most = if bits_set[i] as usize * 2 >= oxy_values.len() {
                '1'
            } else {
                '0'
            };
            oxy_values = oxy_values.iter().filter(|val| val[i] as char == most).copied().collect();
        }
        if co2_values.len() > 1 {
            let bits_set = get_bit_counts(&co2_values);
            let least = if bits_set[i] as usize * 2 >= co2_values.len() {
                '0'
            } else {
                '1'
            };
            co2_values = co2_values.iter().filter(|val| val[i] as char == least).copied().collect();
        }
    }
    let gamma = u32::from_str_radix(&gamma_str, 2).unwrap();
    let epsilon = u32::from_str_radix(&epsilon_str, 2).unwrap();
    println!("gamma: {}, epsilon: {}, gamms * epsilon = {}", gamma, epsilon, gamma * epsilon);
    let oxy = u32::from_str_radix(str::from_utf8(&oxy_values[0]).unwrap(), 2).unwrap();
    let co2 = u32::from_str_radix(str::from_utf8(&co2_values[0]).unwrap(), 2).unwrap();
    println!("oxy: {}, co2: {}, oxy * co2 = {}", oxy, co2, oxy * co2);
}
fn get_bit_counts(input: &[&[u8]]) -> [u32; 12] {
    let mut bits_set = [0u32; 12];
    for line in input {
        for (i, ch) in line.iter().enumerate() {
            if *ch as char == '1' {
                bits_set[i] += 1;
            }
        }
    }
    bits_set
}
