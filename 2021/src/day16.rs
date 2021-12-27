use std::fs;

#[derive(Debug, PartialEq)]
struct Packet {
    v: u8,
    t: u8,
    literal: Option<usize>,
    sub: Box<[Packet]>,
}
impl Packet {
    fn add_versions(&self) -> usize {
        let mut total = self.v as usize;
        for sub in self.sub.iter() {
            total += sub.add_versions();
        }
        total
    }
    
    fn eval(&self) -> usize {
        match self.t {
            0 => self.sub.iter().map(|sub| sub.eval()).sum(),
            1 => self.sub.iter().map(|sub| sub.eval()).product(),
            2 => self.sub.iter().map(|sub| sub.eval()).min().unwrap(),
            3 => self.sub.iter().map(|sub| sub.eval()).max().unwrap(),
            4 => self.literal.unwrap(),
            5 => if self.sub[0].eval() > self.sub[1].eval() { 1 } else { 0 },
            6 => if self.sub[0].eval() < self.sub[1].eval() { 1 } else { 0 },
            7 => if self.sub[0].eval() == self.sub[1].eval() { 1 } else { 0 },
            _ => unreachable!(),
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/16.txt").unwrap();
    //let mut nums: Vec<Vec<i32>> = input.lines().map(|line| line.chars().map(|ch| ch.to_digit(10).unwrap() as i32).collect()).collect();
    //let dims = (nums[0].len(), nums.len());
    let (packet, _) = parse_packet(&hex_str_to_binary(input.trim()));
    
    println!("version sum: {}", packet.add_versions());
    println!("value: {}", packet.eval());
}

fn parse_packet(data: &str) -> (Packet, usize) {
    let v = u8::from_str_radix(&data[0..3], 2).unwrap();
    let t = u8::from_str_radix(&data[3..6], 2).unwrap();
    let mut sub = Vec::new();
    let mut len = 6;
    let literal = if t == 4 {
        let (literal, sub_len) = parse_literal(&data[6..]);
        len += sub_len;
        Some(literal)
    } else {
        len += 1;
        if &data[6..7] == "0" {
            let total_sub_len = usize::from_str_radix(&data[7..22], 2).unwrap();
            len += 15;
            let mut found_sub_len = 0;
            while found_sub_len < total_sub_len {
                let (sub_packet, sub_len) = parse_packet(&data[len..]);
                sub.push(sub_packet);
                found_sub_len += sub_len;
                len += sub_len;
            }
            if found_sub_len != total_sub_len {
                eprintln!("{:?}", sub);
                panic!("found sub len ({}) is not equal expected sub len ({})", found_sub_len, total_sub_len);
            }
        } else {
            let total_sub_count = usize::from_str_radix(&data[7..18], 2).unwrap();
            len += 11;
            let mut found_sub_count = 0;
            while found_sub_count < total_sub_count {
                let (sub_packet, sub_len) = parse_packet(&data[len..]);
                sub.push(sub_packet);
                found_sub_count += 1;
                len += sub_len;
            }
        }
        None
    };
    
    let packet = Packet {
        v,
        t,
        literal,
        sub: sub.into_boxed_slice(),
    };
    (packet, len)
}

fn parse_literal(data: &str) -> (usize, usize) {
    let mut i = 0;
    let mut bits = String::new();
    while &data[i..i+1] == "1" {
        bits += &data[i+1..i+5];
        i += 5;
    }
    bits += &data[i+1..i+5];
    
    let num = usize::from_str_radix(&bits, 2).unwrap();
    (num, i + 5)
}

fn hex_str_to_binary(string: &str) -> String {
    string.chars().map(|ch| format!("{:04b}", ch.to_digit(16).unwrap())).collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_str_to_binary() {
        let data = "01AF";
        assert_eq!(hex_str_to_binary(data), "0000000110101111");
    }

    #[test]
    fn test_parse_literal() {
        let data = "1000101111";
        assert_eq!(parse_literal(data), (31, 10));
    }

    #[test]
    fn test_parse_literal_packet() {
        let data = "0011001000101111";
        let packet = Packet { v: 1, t: 4, literal: Some(31), sub: Box::new([]) };
        assert_eq!(parse_packet(data), (packet, 16));
        let data = "110100101111111000101000";
        let packet = Packet { v: 6, t: 4, literal: Some(2021), sub: Box::new([]) };
        assert_eq!(parse_packet(data), (packet, 21));
    }

    #[test]
    fn test_parse_op_packet() {
        let data = "00111000000000000110111101000101001010010001001000000000";
        let packet = Packet { v: 1, t: 6, literal: None, sub: Box::new([
            Packet { v: 6, t: 4, literal: Some(10), sub: Box::new([]) },
            Packet { v: 2, t: 4, literal: Some(20), sub: Box::new([]) },
        ]) };
        assert_eq!(parse_packet(data), (packet, 49));
        let data = "11101110000000001101010000001100100000100011000001100000";
        let packet = Packet { v: 7, t: 3, literal: None, sub: Box::new([
            Packet { v: 2, t: 4, literal: Some(1), sub: Box::new([]) },
            Packet { v: 4, t: 4, literal: Some(2), sub: Box::new([]) },
            Packet { v: 1, t: 4, literal: Some(3), sub: Box::new([]) },
        ]) };
        assert_eq!(parse_packet(data), (packet, 51));
    }
}
