use crate::Solution;
use crate::utils::iter_parse_u32;
use std::cmp::Ordering;

pub const SOLUTION: Solution = Solution::U32((4643, 21614));

// It is not necessary to parse the whole list of lists of lists etc into a datastructure
// to find the "greatest" of each pair. Instead this could be done while iterating over the input.
// This could be a major perormance improvement but at the moment I do not have the motivation to
// try to do implement this.

#[derive(Debug, PartialEq, Eq, Clone)]
struct List {
    values: Vec<Entry>,
}
impl List {
    fn from_iter(iter: &mut impl Iterator<Item = u8>) -> Option<Self> {
        let mut values = Vec::new();
        
        loop {
            match iter_parse_u32(iter) {
                (Some(num), next) => {
                    values.push(Entry::Value(num));
                    match next {
                        Some(b',') => {}
                        Some(b']') => break,
                        other => panic!("unexpected char after num: {:?}", other),
                    }
                }
                (None, Some(b'[')) => {
                    values.push(Entry::List(List::from_iter(iter)?));
                    match iter.next() {
                        Some(b',') => {}
                        Some(b']') => break,
                        other => panic!("unexpected char after sub-list: {:?}", other),
                    }
                }
                (None, Some(b']')) => break,
                (None, other) => panic!("unexpected char {:?}", other),
            }
        }
        
        Some(Self { values })
    }
    
    fn cmp(&self, other: &Self) -> Ordering {
        let mut iter_a = self.values.iter();
        let mut iter_b = other.values.iter();
        loop {
            match (iter_a.next(), iter_b.next()) {
                (Some(a), Some(b)) => {
                    match a.cmp(b) {
                        Ordering::Equal => {},
                        other => return other,
                    }
                }
                (None, Some(_)) => return Ordering::Less,
                (Some(_), None) => return Ordering::Greater,
                (None, None) => return Ordering::Equal,
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Entry {
    Value(u32),
    List(List),
}
impl Entry {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Value(a), Self::Value(b)) => a.cmp(b),
            (Self::List(a), Self::List(b)) => a.cmp(b),
            (Self::Value(a), Self::List(list)) => cmp_value_to_list(*a, list),
            (list, value) => value.cmp(list).reverse(),
        }
    }
}

fn cmp_value_to_list(value: u32, mut list: &List) -> Ordering {
    let mut list_has_second_el = Ordering::Equal;
    while let Some(first) = list.values.first() {
        if list.values.len() >= 2 {
            list_has_second_el = Ordering::Less;
        }
        match first {
            Entry::Value(b) => return value.cmp(&b).then(list_has_second_el),
            Entry::List(b) => list = b,
        }
    }
    Ordering::Greater
}

pub fn solve(input: String) -> Solution {
    let mut lists = Vec::new();
    
    for line in input.into_bytes().split(|&c| c == b'\n') {
        if line.is_empty() {
            continue;
        }
        let mut line_iter = line.iter().copied();
        assert_eq!(line_iter.next(), Some(b'['));
        let list = List::from_iter(&mut line_iter).unwrap();
        lists.push(list);
    }
    
    let mut sum = 0;
    let mut i = 1;
    for pair in lists.chunks_exact(2) {
        if pair[0].cmp(&pair[1]) != Ordering::Greater {
            sum += i;
        }
        i += 1;
    }
    
    let mut less_than_2 = 1;
    let mut less_than_6 = 2; // the inserted divider [[2]] is also less than [[6]]
    for list in lists.iter() {
        if cmp_value_to_list(6, &list) == Ordering::Greater {
            less_than_6 += 1;
            if cmp_value_to_list(2, &list) == Ordering::Greater {
                less_than_2 += 1;
            }
        }
    }
    
    (sum as u32, less_than_2 * less_than_6).into()
}
