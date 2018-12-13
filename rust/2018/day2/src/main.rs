use std::fs::File;
use std::io::{BufRead, BufReader, Result};
use std::collections::BTreeMap;

fn main() -> Result<()> {
    let lines = read_lines("input")?;
    println!("Part1 answer: {}", part1_checksum(&lines).unwrap());
    println!("Part2 answer: {}", part2_mirrored_chars(&lines).unwrap());
    Ok(())
}

fn part1_checksum(lines: &Vec<String>) -> Result<u32> {
    let mut doubles = 0u32;
    let mut triples = 0u32;
    for line in lines {
        let score = part1_score_line(&line);
        doubles += score.0;
        triples += score.1;
    }
    Ok(doubles * triples)
}

fn part1_score_line(line: &str) -> (u32, u32) {
    let mut map = BTreeMap::new();
    for c in line.chars() {
        *map.entry(c).or_insert(0) += 1;
    }
    let double = map.values().fold(0, |a, &i| if i == 2 { 1 } else { a });
    let triple = map.values().fold(0, |a, &i| if i == 3 { 1 } else { a });
    (double, triple)
}

// Finds the common chars between the two lines that differ only
// by one character in the same position
// "ABCD" and "ABED" would return "ABD"
fn part2_mirrored_chars(lines: &Vec<String>) -> Option<String> {
    for ref line1 in lines {
        for ref line2 in lines {
            let zipped = line1.chars().zip(line2.chars());
            let mirrored: String = zipped.filter(|p| p.0 == p.1).map(|p| p.0).collect();
            if line1.len() == mirrored.len() + 1 {
                return Some(mirrored);
            }
        }
    }
    None
}

fn read_lines(filepath: &str) -> Result<Vec<String>> {
    let f = File::open(filepath)?;
    let mut lines: Vec<String> = Vec::new();
    for line in BufReader::new(f).lines() {
        lines.push(line.unwrap());
    }
    Ok(lines)
}
