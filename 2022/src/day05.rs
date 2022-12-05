use crate::utils::iter_parse_u32;

pub fn solve(input: String) -> (String, String) {
    let bytes = input.as_bytes();
    
    let mut line_len = 0;
    while bytes[line_len] != b'\n' {
        line_len += 1;
    }
    line_len += 1;
    
    let mut stack_len = 0;
    while bytes[stack_len * line_len + 1] != b'1' {
        stack_len += 1;
    }
    
    let mut stacks = Vec::with_capacity(line_len / 4);
    assert!(stacks.len() < 10, "code only works if stack numbers are single digit");
    let mut i = 1;
    while i < line_len {
        let mut stack = Vec::new();
        for h in (0..stack_len).rev() {
            let val = bytes[h * line_len + i];
            if val == b' ' {
                break;
            }
            stack.push(val);
        }
        stacks.push(stack);
        i += 4;
    }
    
    let mut stacks2 = stacks.clone();
    
    // I tried to reserve enough capacity for all stacks here to prevent reallocation
    // but it does not seem to make a difference
    
    let mut iter = bytes[(stack_len + 1) * line_len..].iter().copied();
    while iter.nth(b"\nmove ".len() - 1).is_some() {
        let count = iter_parse_u32(&mut iter).0.unwrap() as usize;
        let from = (iter.nth(b"from ".len()).unwrap() - b'1') as usize;
        let to = (iter.nth(b" to ".len()).unwrap() - b'1') as usize;
        
        for _ in 0..count {
            let val = stacks[from].pop().unwrap();
            stacks[to].push(val);
        }
        
        if from < to {
            let (left, right) = stacks2.split_at_mut(to);
            let drain_from = left[from].len() - count;
            right[0].extend(left[from].drain(drain_from..));
        } else if to < from {
            let (left, right) = stacks2.split_at_mut(from);
            let drain_from = right[0].len() - count;
            left[to].extend(right[0].drain(drain_from..));
        } else {
            unreachable!();
        }
    }
    
    (stacks_to_str(stacks), stacks_to_str(stacks2))
}

fn stacks_to_str(stacks: Vec<Vec<u8>>) -> String {
    stacks.into_iter()
        .map(|mut stack| char::from_u32(stack.pop().unwrap_or(b'_') as u32).unwrap())
        .collect::<String>()
}
