use crate::Solution;
use crate::utils::iter_parse_i32;

pub const SOLUTION: Solution = Solution::U64((4151, 7848878698663));

pub fn solve(input: String) -> Solution {
    let mut iter = input.bytes();
    let mut zero_pos = 0;
    let mut tree = Tree::new();
    tree.append(Node::new(iter_parse_i32(&mut iter).0.unwrap() as i64));
    while let Some(num) = iter_parse_i32(&mut iter).0 {
        if num == 0 {
            zero_pos = tree.nodes.len();
        }
        tree.append(Node::new(num as i64));
    }
    
    let mut tree2 = tree.clone();
    for node in tree2.nodes.iter_mut() {
        node.value *= 811589153;
    }
    
    mix_list(&mut tree);
    let res1 = (1..=3).map(|n| list_nth(&tree, zero_pos, n * 1000)).sum::<i64>();
    
    for _ in 0..10 {
        mix_list(&mut tree2);
    }
    let res2 = (1..=3).map(|n| list_nth(&tree2, zero_pos, n * 1000)).sum::<i64>();
    
    (res1 as u64, res2 as u64).into()
}

fn mix_list(tree: &mut Tree) {
    for i in 0..tree.nodes.len() {
        let value = tree.nodes[i].value;
        tree.move_around(i, value);
    }
}

fn list_nth(tree: &Tree, start: usize, n: usize) -> i64 {
    let start_idx = tree.index_of(start);
    return *tree.get((start_idx + n) % tree.nodes.len()).unwrap();
}

#[derive(Clone)]
struct Tree {
    root: Option<usize>,
    nodes: Vec<Node>,
}

impl Tree {
    fn new() -> Self {
        Self { nodes: Vec::new(), root: None }
    }
    
    fn get(&self, mut index: usize) -> Option<&i64> {
        if let Some(mut node) = self.root {
            loop {
                let count_l = self.nodes[node].count_l;
                let count_r = self.nodes[node].count_r;
                if index < count_l {
                    node = self.nodes[node].left.unwrap();
                } else if index == count_l {
                    break Some(&self.nodes[node].value);
                } else if index <= count_l + count_r {
                    index -= count_l + 1;
                    node = self.nodes[node].right.unwrap();
                } else {
                    break None;
                }
            }
        } else {
            None
        }
    }
    
    fn index_of(&self, insert_idx: usize) -> usize {
        let mut index = self.nodes[insert_idx].count_l;
        let mut parent = self.nodes[insert_idx].parent;
        let mut child = insert_idx;
        while child != parent {
            if self.nodes[parent].left == Some(child) {
                // nothing
            } else if self.nodes[parent].right == Some(child) {
                index += self.nodes[parent].count_l + 1;
            } else {
                unreachable!();
            }
            child = parent;
            parent = self.nodes[parent].parent;
        }
        index
    }
    
    fn remove_keep(&mut self, insert_idx: usize) -> usize {
        let count_l = self.nodes[insert_idx].count_l;
        let mut old_idx = count_l;
        let mut parent = self.nodes[insert_idx].parent;
        let mut child = insert_idx;
        while child != parent {
            if self.nodes[parent].left == Some(child) {
                self.nodes[parent].count_l -= 1;
            } else if self.nodes[parent].right == Some(child) {
                self.nodes[parent].count_r -= 1;
                old_idx += self.nodes[parent].count_l + 1;
            } else {
                unreachable!();
            }
            child = parent;
            parent = self.nodes[parent].parent;
        }
        
        let parent = self.nodes[insert_idx].parent;
        let new = match (self.nodes[insert_idx].left, self.nodes[insert_idx].right) {
            (Some(left), Some(right)) => {
                let mut first = right;
                while let Some(child) = self.nodes[first].left {
                    self.nodes[first].count_l += count_l;
                    first = child;
                }
                self.nodes[first].count_l += count_l;
                self.nodes[first].left = Some(left);
                self.nodes[left].parent = first;
                Some(right)
            }
            (Some(child), None) | (None, Some(child)) => Some(child),
            _ => None,
        };
        self.nodes[insert_idx].left = None;
        self.nodes[insert_idx].right = None;
        self.nodes[insert_idx].parent = insert_idx;
        
        if insert_idx != parent {
            if let Some(new) = new {
                self.nodes[new].parent = parent;
            }
            if self.nodes[parent].left == Some(insert_idx) {
                self.nodes[parent].left = new;
            } else if self.nodes[parent].right == Some(insert_idx) {
                self.nodes[parent].right = new;
            } else {
                unreachable!();
            }
        } else {
            assert_eq!(self.root, Some(parent));
            if let Some(new) = new {
                self.nodes[new].parent = new;
                self.root = Some(new);
            } else {
                self.root = None;
            }
        }
        
        old_idx
    }
    
    fn move_around(&mut self, insert_idx: usize, amount: i64) {
        let old_idx = self.remove_keep(insert_idx);
        let mut index = (old_idx as i64 + amount).rem_euclid((self.nodes.len() - 1) as i64) as usize;
        let mut node = self.root.unwrap();
        loop {
            let count_l = self.nodes[node].count_l;
            let count_r = self.nodes[node].count_r;
            if index < count_l {
                node = self.nodes[node].left.unwrap();
            } else if index == count_l {
                break;
            } else if index <= count_l + count_r {
                index -= count_l + 1;
                node = self.nodes[node].right.unwrap();
            } else {
                unreachable!();
            }
        }
        let mut parent = self.nodes[node].parent;
        if node != parent {
            if self.nodes[parent].left == Some(node) {
                self.nodes[parent].left = Some(insert_idx);
            } else if self.nodes[parent].right == Some(node) {
                self.nodes[parent].right = Some(insert_idx);
            } else {
                unreachable!();
            }
        } else {
            assert_eq!(self.root, Some(parent));
            self.root = Some(insert_idx);
            parent = insert_idx;
        }
        if let Some(left) = self.nodes[node].left {
            self.nodes[left].parent = insert_idx;
        }
        self.nodes[insert_idx].parent = parent;
        self.nodes[insert_idx].left = self.nodes[node].left;
        self.nodes[insert_idx].count_l = self.nodes[node].count_l;
        self.nodes[insert_idx].right = Some(node);
        self.nodes[insert_idx].count_r = self.nodes[node].count_r + 1;
        self.nodes[node].parent = insert_idx;
        self.nodes[node].left = None;
        self.nodes[node].count_l = 0;
        
        let mut child = insert_idx;
        while child != parent {
            if self.nodes[parent].left == Some(child) {
                self.nodes[parent].count_l += 1;
            } else if self.nodes[parent].right == Some(child) {
                self.nodes[parent].count_r += 1;
            } else {
                unreachable!();
            }
            child = parent;
            parent = self.nodes[child].parent;
        }
        
    }
    
    fn append(&mut self, mut node: Node) {
        if let Some(mut parent) = self.root {
            loop {
                if self.nodes[parent].count_r <= self.nodes[parent].count_l {
                    self.nodes[parent].count_r += 1;
                    match self.nodes[parent].right {
                        Some(child) => {
                            parent = child;
                        }
                        None => {
                            node.parent = parent;
                            self.nodes[parent].right = Some(self.nodes.len());
                            self.nodes.push(node);
                            break;
                        }
                    }
                } else if let Some(child) = self.nodes[parent].right {
                    let grandparent = self.nodes[parent].parent;
                    if grandparent != parent {
                        self.nodes[grandparent].right = Some(child);
                        self.nodes[child].parent = grandparent;
                    } else {
                        assert_eq!(self.root, Some(parent));
                        self.root = Some(child);
                        self.nodes[child].parent = child;
                    }
                    
                    if let Some(left) = self.nodes[child].left {
                        self.nodes[left].parent = parent;
                    }
                    self.nodes[parent].right = self.nodes[child].left;
                    self.nodes[parent].count_r = self.nodes[child].count_l;
                    self.nodes[parent].parent = child;
                    self.nodes[child].left = Some(parent);
                    self.nodes[child].count_l = self.nodes[parent].count_l + self.nodes[parent].count_r + 1;
                    parent = child;
                } else {
                    unreachable!();
                }
            }
        } else {
            node.parent = 0;
            self.nodes.push(node);
            self.root = Some(0);
        }
        
    }
}

#[derive(Clone)]
struct Node {
    value: i64,
    parent: usize,
    left: Option<usize>,
    right: Option<usize>,
    count_l: usize,
    count_r: usize,
}

impl Node {
    fn new(value: i64) -> Self {
        Self { value, parent: 0, left: None, right: None, count_l: 0, count_r: 0 }
    }
}
