use std::collections::HashSet;
use std::fs;

const START: usize = 0;
const END: usize = 1;

pub fn solve() {
    let input = fs::read_to_string("inputs/12.txt").unwrap();
    let mut nodes: Vec<&str> = vec!["start", "end"];
    let mut is_big = vec![false, false];
    let mut edges = vec![vec![], vec![]];
    for conn in input.lines() {
        let mut splitted = conn.split('-');
        
        let a = splitted.next().unwrap();
        let a_pos = nodes.iter().position(|node| node == &a);
        let a_pos = if let Some(a_pos) = a_pos {
            a_pos
        } else {
            is_big.push(a.chars().next().unwrap().is_ascii_uppercase());
            nodes.push(a);
            edges.push(vec![]);
            nodes.len() - 1
        };
        
        let b = splitted.next().unwrap();
        let b_pos = nodes.iter().position(|node| node == &b);
        let b_pos = if let Some(b_pos) = b_pos {
            b_pos
        } else {
            is_big.push(b.chars().next().unwrap().is_ascii_uppercase());
            nodes.push(b);
            edges.push(vec![]);
            nodes.len() - 1
        };
        
        edges[a_pos].push(b_pos);
        edges[b_pos].push(a_pos);
    }
    
    let mut pathes: HashSet<Vec<usize>> = HashSet::new();
    find_pathes(vec![START], &mut pathes, &edges, &is_big);
    let mut pathes_2: HashSet<Vec<usize>> = HashSet::new();
    find_pathes_2(vec![START], false, &mut pathes_2, &edges, &is_big);
    
    println!("path count: {}", pathes.len());
    println!("path count 2: {}", pathes_2.len());
    // for path in pathes {
        // println!("{}", path.iter().map(|&node| nodes[node]).collect::<Vec<&str>>().join("-"));
    // }
}

fn find_pathes(path: Vec<usize>, pathes: &mut HashSet<Vec<usize>>, edges: &[Vec<usize>], is_big: &[bool]) {
    let node = *path.last().unwrap();
    if node == END {
        if !pathes.contains(&path) {
            pathes.insert(path);
        }
    } else {
        for &edge in edges[node].iter() {
            if is_big[edge] || !path.contains(&edge) {
                let mut new_path = path.clone();
                new_path.push(edge);
                find_pathes(new_path, pathes, edges, is_big);
            }
        }
    }
}

fn find_pathes_2(path: Vec<usize>, twice: bool, pathes: &mut HashSet<Vec<usize>>, edges: &[Vec<usize>], is_big: &[bool]) {
    let node = *path.last().unwrap();
    if node == END {
        if !pathes.contains(&path) {
            pathes.insert(path);
        }
    } else {
        for &edge in edges[node].iter() {
            if is_big[edge] || !path.contains(&edge) {
                let mut new_path = path.clone();
                new_path.push(edge);
                find_pathes_2(new_path, twice, pathes, edges, is_big);
            } else if !twice && edge != START {
                let mut new_path = path.clone();
                new_path.push(edge);
                find_pathes_2(new_path, true, pathes, edges, is_big);
            }
        }
    }
}
