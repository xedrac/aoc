use std::fs::File;
use std::io::{BufRead, BufReader, Result};
use std::collections::HashMap;

fn main() {
    let deltas = parse_deltas_file("input").unwrap_or(vec![0]);
    part1_calculate_freq(&deltas);
    part2_find_reached_twice_freq(&deltas);
}

fn part1_calculate_freq(deltas: &Vec<i32>) {
    let freq: i32 = deltas.iter().sum();
    println!("Part1 answer: {}", freq);
}

fn part2_find_reached_twice_freq(deltas: &Vec<i32>) {
    let mut visited = HashMap::new();
    let mut freq = 0i32;
    visited.insert(freq, true);

    'outer: loop {
        for d in deltas {
            freq += d;
            if visited.contains_key(&freq) {
                break 'outer;
            }
            visited.insert(freq, true);
        }
    }
    println!("Part2 answer: {}", freq);
}

fn parse_deltas_file(filepath: &str) -> Result<Vec<i32>> {
    let f = File::open(filepath)?;
    let mut deltas: Vec<i32> = Vec::new();
    for line in BufReader::new(f).lines() {
        let value = &line.unwrap().parse::<i32>().unwrap_or(0);
        deltas.push(*value);
    }
    Ok(deltas)
}
