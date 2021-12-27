use std::fs;

const DARK: char = '.';
const LIGHT: char = '#';

pub fn solve() {
    let input = fs::read_to_string("inputs/20.txt").unwrap();
    
    let empty_vec = vec![];
    let mut lines = input.lines();
    let algo = lines.next().unwrap().chars().collect::<Vec<char>>();
    lines.next(); //empty input line
    let mut image = lines.map(|line| {
        let mut row = line.chars().collect::<Vec<char>>();
        row.insert(0, DARK);
        row.insert(0, DARK);
        row.push(DARK);
        row.push(DARK);
        row
    }).collect::<Vec<Vec<char>>>();
    let mut dark_row = vec![];
    dark_row.resize(image[0].len(), DARK);
    image.insert(0, dark_row.clone());
    image.insert(0, dark_row.clone());
    image.push(dark_row.clone());
    image.push(dark_row);
    
    for step in 0..50 {
        let mut new_image = Vec::with_capacity(image.len() + 2);
        let mut dark_row = vec![];
        let width = image[0].len();
        let empty_color = if step % 2 == 0 { LIGHT } else { DARK };
        dark_row.resize(width + 2, empty_color);
        new_image.push(dark_row.clone());
        new_image.push(dark_row.clone());
        for j in 1..image.len()-1 {
            let mut new_row = Vec::with_capacity(width + 2);
            new_row.push(empty_color);
            new_row.push(empty_color);
            for i in 1..width-1 {
                let mut num = map_color(*image.get(j - 1).unwrap_or(&empty_vec).get(i - 1).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j - 1).unwrap_or(&empty_vec).get(i).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j - 1).unwrap_or(&empty_vec).get(i + 1).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j).unwrap_or(&empty_vec).get(i - 1).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j).unwrap_or(&empty_vec).get(i).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j).unwrap_or(&empty_vec).get(i + 1).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j + 1).unwrap_or(&empty_vec).get(i - 1).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j + 1).unwrap_or(&empty_vec).get(i).unwrap_or(&DARK));
                num = num << 1;
                num += map_color(*image.get(j + 1).unwrap_or(&empty_vec).get(i + 1).unwrap_or(&DARK));
                new_row.push(algo[num]);
            }
            new_row.push(empty_color);
            new_row.push(empty_color);
            new_image.push(new_row);
        }
        new_image.push(dark_row.clone());
        new_image.push(dark_row);
        image = new_image;
    }
    
    let mut light_count: u32 = 0;
    for row in image.iter() {
        for pixel in row.iter() {
            if *pixel == LIGHT {
                light_count += 1;
            }
        }
        // println!("{}", row.iter().collect::<String>());
    }
    println!("light count: {}", light_count);
}

fn map_color(ch: char) -> usize {
    match ch {
        DARK => 0,
        LIGHT => 1,
        _ => unreachable!(),
    }
}
