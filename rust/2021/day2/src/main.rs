use std::io::{BufReader, BufRead, Lines};
use std::fs::File;

fn main() {
    let file = File::open("input").expect("Unable to read input file");
    let lines = BufReader::new(file).lines();

    //let (horiz, depth) = move_submarine_simple(lines);
    //println!("horiz: {}, depth: {},  product: {}", horiz, depth, horiz*depth);

    let (horiz, depth) = move_submarine_complex(lines);
    println!("horiz: {}, depth: {},  product: {}", horiz, depth, horiz*depth);
}

fn move_submarine_simple(lines: Lines<BufReader<File>>) -> (u64, u64) {
    let mut horiz = 0;
    let mut depth = 0;
    for line in lines {
        if let Ok(text) = line {
            let tokens: Vec<&str> = text.split_ascii_whitespace().collect();
            assert!(tokens.len() == 2);
            match tokens[0] {
                "forward" => horiz += tokens[1].parse::<u64>().expect("failed to parse forward value"),
                "down" => depth += tokens[1].parse::<u64>().expect("failed to parse down value"),
                "up" => depth -= tokens[1].parse::<u64>().expect("failed to parse up value"),
                _ => panic!("Invalid command"),
            };
        }
    }
    (horiz, depth)
}

fn move_submarine_complex(lines: Lines<BufReader<File>>) -> (u64, u64) {
    let mut horiz = 0;
    let mut depth = 0;
    let mut aim: i64 = 0;
    for line in lines {
        if let Ok(text) = line {
            let tokens: Vec<&str> = text.split_ascii_whitespace().collect();
            assert!(tokens.len() == 2);
            let value = tokens[1].parse::<u64>().expect("failed to parse forward value");
            match tokens[0] {
                "forward" => {
                    horiz += value;
                    depth += aim * value as i64;
                }
                "down" => aim += value as i64,
                "up" => aim -= value as i64,
                _ => panic!("Invalid command"),
            };
        }
    }
    (horiz, depth as u64)
}
