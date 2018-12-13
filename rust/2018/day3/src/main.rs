#[macro_use]
extern crate scan_fmt;

use std::fs::File;
use std::io::{BufRead, BufReader, Result};
use std::collections::HashMap;

type FabricMap = HashMap<(u32, u32), u32>;

fn main() -> Result<()> {
    let lines = read_lines("input")?;
    let map = build_map(&lines);
    println!("Part1 answer: {}", part1_overlapping_area(&map));
    println!(
        "Part2 answer: {}",
        part2_find_disjoint_id(&lines, &map).unwrap()
    );
    Ok(())
}

fn part1_overlapping_area(map: &FabricMap) -> u32 {
    // Return number of coordinates that were touched 2 or more times
    map.values().fold(0, |a, &i| if i >= 2 { a + 1 } else { a })
}

// Return the ID of the claim that doesn't overlap with any other claims
fn part2_find_disjoint_id(lines: &Vec<String>, map: &FabricMap) -> Option<u32> {
    'outer: for line in lines {
        let (id, x0, y0, w, h) = parse_line(line);
        for y in y0..y0 + h {
            for x in x0..x0 + w {
                if *map.get(&(x, y)).unwrap() != 1 {
                    continue 'outer;
                }
            }
        }
        return Some(id);
    }
    None
}

fn build_map(lines: &Vec<String>) -> FabricMap {
    let mut map: FabricMap = HashMap::new();
    for line in lines {
        let (_id, x0, y0, w, h) = parse_line(line);
        for y in y0..y0 + h {
            for x in x0..x0 + w {
                *map.entry((x, y)).or_insert(0) += 1;
            }
        }
    }
    map
}

fn parse_line(line: &str) -> (u32, u32, u32, u32, u32) {
    let (id, l, t, w, h) = scan_fmt!(line, "#{} @ {},{}: {}x{}", u32, u32, u32, u32, u32);
    (id.unwrap(), l.unwrap(), t.unwrap(), w.unwrap(), h.unwrap())
}

fn read_lines(filepath: &str) -> Result<Vec<String>> {
    let f = File::open(filepath)?;
    let mut lines: Vec<String> = Vec::new();
    for line in BufReader::new(f).lines() {
        lines.push(line.unwrap());
    }
    Ok(lines)
}
