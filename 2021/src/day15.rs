use std::fs;
use std::cmp::Reverse;
use priority_queue::PriorityQueue;

#[derive(PartialEq, Eq, Hash)]
struct Node {
    id: usize,
    dist: usize,
    ancestor: Option<usize>,
    weight: usize,
    neighbors: Vec<usize>,
}
impl Node {
    fn relax(id: usize, nodes: &mut [Node], queue: &mut PriorityQueue<usize, Reverse<usize>>) {
        let self_dist = nodes[id].dist;
        let neighbors: &Vec<usize> = unsafe { (&nodes[id].neighbors as *const Vec<usize>).as_ref().unwrap() };
        for neighbor in neighbors.iter() {
            let node = &mut nodes[*neighbor];
            if node.dist > self_dist + node.weight {
                node.dist = self_dist + node.weight;
                queue.change_priority(neighbor, Reverse(self_dist + node.weight));
                node.ancestor = Some(id);
            }
        }
    }
}

pub fn solve() {
    let input = fs::read_to_string("inputs/15.txt").unwrap();
    let mut nums: Vec<Vec<usize>> = input.lines().map(|line| line.chars().map(|ch| ch.to_digit(10).unwrap() as usize).collect()).collect();
    let dims = (nums[0].len(), nums.len());
    
    dijkstra(&nums, dims);
    
    for row in nums.iter_mut() {
        for i in 1..5 {
            row.append(&mut row.iter().take(dims.0).map(|x| if x + i < 10 { x + i } else { x + i - 9 }).collect::<Vec<usize>>());
        }
    }
    for i in 1..5 {
        for j in 0..dims.1 {
            nums.push(nums[j].iter().map(|x| if x + i < 10 { x + i } else { x + i - 9 }).collect::<Vec<usize>>());
        }
    }
    let dims = (dims.0 * 5, dims.1 * 5);
    
    dijkstra(&nums, dims);
}

fn dijkstra(nums: &[Vec<usize>], dims: (usize, usize)) {
    let mut nodes = vec![];
    for j in 0..dims.1 {
        for i in 0..dims.0 {
            nodes.push(Node {
                id: i + j * dims.0,
                dist: usize::MAX,
                ancestor: None,
                weight: nums[j][i],
                neighbors: vec![],
            });
        }
    }
    for j in 0..dims.1 {
        for i in 0..dims.0 {
            let node = &mut nodes[i + j * dims.0];
            if i != 0 { node.neighbors.push((i - 1) + j * dims.0); }
            if i != dims.0 - 1 { node.neighbors.push((i + 1) + j * dims.0); }
            if j != 0 { node.neighbors.push(i + (j - 1) * dims.0); }
            if j != dims.1 - 1 { node.neighbors.push(i + (j + 1) * dims.0); }
        }
    }
    
    nodes[0].dist = 0;
    let mut queue = PriorityQueue::new();
    for node in nodes.iter() {
        queue.push(node.id, Reverse(node.dist));
    }
        
    while let Some(min_node) = queue.pop() {
        let node_id = min_node.0;
        if node_id == nodes.len() - 1 {
            break;
        }
        
        Node::relax(node_id, &mut nodes, &mut queue);
    }
    
    let mut node = nodes.last().unwrap();
    let mut risk = node.weight;
    while let Some(prev) = node.ancestor {
        risk += nodes[prev].weight;
        node = &nodes[prev];
    }
    
    println!("risk: {}", risk - nodes[0].weight);
}
