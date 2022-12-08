use std::env;
use std::fs;
use std::io::{stdout, Write};
use std::time::Instant;

use adventofcode_2022::DAYS;

fn main() {
    let args: Vec<String> = env::args().collect();
    let day = args.get(1).expect("expected one argument");
    
    let range = if day == "all" {
        0..DAYS.len()
    } else if let Ok(day) = day.parse::<usize>() {
        if day == 0 || day > DAYS.len() {
            println!("There is no day {}. Provide a day between 1 and {} inclusive.", day, DAYS.len());
            return;
        }
        day - 1..day
    } else {
        println!("Please provide as argument 'all' or the specific day you want to run.");
        return;
    };
    
    print!("reading input files ...");
    stdout().flush().unwrap();
    let start = Instant::now();
    let inputs = range.map(|day| (day, fs::read_to_string(format!("inputs/{:0>2}.txt", day + 1)).unwrap())).collect::<Vec<_>>();
    let time_input = start.elapsed();
    println!(" took {:?}", time_input);
    
    print!("computing solutions ...");
    stdout().flush().unwrap();
    let start = Instant::now();
    let solutions = inputs.into_iter().map(|(day, input)| {
        let start = Instant::now();
        let sol = DAYS[day].0(input);
        let time = start.elapsed();
        (day, sol, time)
    }).collect::<Vec<_>>();
    let time_compute = start.elapsed();
    println!(" took {:?}", time_compute);
    
    println!("printing solutions:");
    let start = Instant::now();
    for (day, sol, time) in solutions.into_iter() {
        println!("- Day {} ({:?}): {}", day + 1, time, sol);
        let expected = &DAYS[day].1;
        if &sol != expected {
            println!("  -> solutions are wrong, schould be: {}", expected);
        }
    }
    let time_output = start.elapsed();
    
    println!("Timings:");
    println!("- input: {:?}", time_input);
    println!("- compute: {:?}", time_compute);
    println!("- output: {:?}", time_output);
}
