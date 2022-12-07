use crate::utils::iter_parse_u32;
use std::str;

struct FileSystem {
    dirs: Vec<Dir>,
    curr: usize,
}
impl FileSystem {
    fn new() -> Self {
        let mut dirs = Vec::with_capacity(16);
        dirs.push(Dir::new(0));
        Self { dirs, curr: 0 }
    }
    
    fn to_top(&mut self) {
        self.curr = 0;
    }
    
    fn dir_up(&mut self) {
        self.curr = self.dirs[self.curr].parent;
    }
    
    fn dir_down(&mut self) {
        let new_dir = Dir::new(self.curr);
        let dir_id = self.dirs.len();
        self.dirs[self.curr].children.push(dir_id);
        self.dirs.push(new_dir);
        self.curr = dir_id;
    }
    
    fn add_file(&mut self, size: u32) {
        self.dirs[self.curr].file_size += size;
    }
    
    fn reserve_sub_dir_space(&mut self, count: usize) {
        self.dirs[self.curr].children.reserve_exact(count);
    }
    
    fn sum_small_dirs(&mut self, max: u32, stack: &mut Vec<usize>) -> u32 {
        let mut sum = 0;
        stack.clear();
        stack.push(0);
        while let Some(dir_id) = stack.pop() {
            let size = match self.dirs[dir_id].sub_dir_size {
                u32::MAX => {
                    self.dirs[dir_id].sub_dir_size = 0;
                    let children = &self.dirs[dir_id].children;
                    if !children.is_empty() {
                        stack.push(dir_id);
                        stack.extend(children.iter());
                        continue;
                    }
                    self.dirs[dir_id].file_size
                }
                size => size + self.dirs[dir_id].file_size,
            };
            
            let parent_id = self.dirs[dir_id].parent;
            self.dirs[parent_id].sub_dir_size += size;
            
            if size <= max {
                sum += size;
            }
        }
        sum
    }
    
    fn find_smallest_with_at_least(&self, at_least: u32, stack: &mut Vec<usize>) -> u32 {
        let mut min = u32::MAX;
        stack.clear();
        stack.push(0);
        while let Some(dir_id) = stack.pop() {
            let file_size = self.dirs[dir_id].file_size;
            let sub_dir_size = self.dirs[dir_id].sub_dir_size;
            let total_size = file_size + sub_dir_size;
            
            if total_size < min && total_size >= at_least {
                min = total_size;
            }
            
            if sub_dir_size >= at_least {
                stack.extend(self.dirs[dir_id].children.iter());
            }
        }
        min
    }
}

struct Dir {
    parent: usize,
    children: Vec<usize>,
    file_size: u32,
    sub_dir_size: u32,
}
impl Dir {
    fn new(parent: usize) -> Self {
        Dir { parent, children: Vec::new(), file_size: 0, sub_dir_size: u32::MAX }
    }
}

pub fn solve(input: String) -> (u32, u32) {
    let mut file_system = FileSystem::new();
    let mut total_size = 0;
    let mut sub_dir_count = 0;
    
    let mut iter = input.as_bytes().split(|&c| c == b' ' || c == b'\n');
    loop {
        let first = iter.next().unwrap();
        match first.get(0) {
            Some(b'$') => {
                debug_assert_eq!(first, b"$");
                
                if sub_dir_count > 0 {
                    file_system.reserve_sub_dir_space(sub_dir_count);
                    sub_dir_count = 0;
                }
                
                let cmd = iter.next().unwrap();
                match cmd.get(0) {
                    Some(b'c') => {
                        debug_assert_eq!(cmd, b"cd");
                        let name = iter.next().unwrap();
                        match name.get(0) {
                            Some(b'.') => file_system.dir_up(),
                            Some(b'/') => file_system.to_top(),
                            _ =>  file_system.dir_down(),
                        }
                    }
                    Some(b'l') => {
                        debug_assert_eq!(cmd, b"ls");
                        debug_assert_eq!(file_system.dirs[file_system.curr].file_size, 0);
                    }
                    _ => panic!("unknown command {}", str::from_utf8(cmd).unwrap()),
                }
            }
            Some(b'd') => {
                debug_assert_eq!(first, b"dir");
                let dir_name = iter.next();
                debug_assert!(dir_name.is_some());
                sub_dir_count += 0;
            }
            Some(c) => {
                debug_assert!(c.is_ascii_digit());
                let size = iter_parse_u32(&mut first.iter().copied()).0.unwrap();
                let file_name = iter.next();
                debug_assert!(file_name.is_some());
                file_system.add_file(size);
                total_size += size;
            }
            None => {
                break;
            }
        }
    }
    
    let free = 70000000_u32.checked_sub(total_size).unwrap();
    let needed = 30000000_u32.checked_sub(free).unwrap();
    let mut buf = Vec::new();
    
    (file_system.sum_small_dirs(100000, &mut buf), file_system.find_smallest_with_at_least(needed, &mut buf))
}
