#![allow(dead_code)]
use anyhow::Result;
use std::collections::HashMap;
use itertools::Itertools;

#[derive(Debug)]
struct Entry {
    patterns: Vec<String>,
    outputs: Vec<String>,
}       

fn decode_seq(encoded_seq: &str, decode_map: &HashMap<char, char>) -> Option<u8> {
    let decoded = encoded_seq.chars().map(|c| decode_map.get(&c).unwrap()).sorted().collect::<String>();
    //println!("encoded: {},  decoded: {}", encoded_seq, decoded);
    match decoded.as_str() {
        "abcefg" => Some(0),
        "cf" | "fc" => Some(1),
        "acdeg" => Some(2),
        "acdfg" => Some(3),
        "bcdf" => Some(4),
        "abdfg" => Some(5),
        "abdefg" => Some(6),
        "acf" => Some(7),
        "abcdefg" => Some(8),
        "abcdfg" => Some(9),
        _ => None,
    }
}

fn run_brute_force_method(entries: &[Entry]) -> Option<u64> {
    let mut sum: u64 = 0;
    let sides = "abcdefg";

    for entry in entries {
        let mut found = false;
        for perm in sides.chars().permutations(sides.len()).unique() {
            let map: HashMap<char, char> = [
                (perm[0], 'a'),
                (perm[1], 'b'),
                (perm[2], 'c'),
                (perm[3], 'd'),
                (perm[4], 'e'),
                (perm[5], 'f'),
                (perm[6], 'g'),
            ].iter().cloned().collect();
            
            if entry.patterns.iter().map(|p| decode_seq(&p, &map)).all(|x| x.is_some()) {
                println!("Found Decode Map: {:?}", map);
                // TODO: decode outputs and add to sum
                sum += decode_outputs(&entry.outputs, &map);
                found = true;
                break;
            }
        }
        if !found {
            eprintln!("No decode map found for: {:?}", entry);
            return None;
        }
    }
    Some(sum)
}

// Given the correct decode map, return the 4-digit number represented by the 4 output sequences
fn decode_outputs(output_seqs: &[String], decode_map: &HashMap<char, char>) -> u64 {
    let mut number = 0u64;
    for i in 0..4 {
        let digit = decode_seq(&output_seqs[i], decode_map).unwrap(); 
        number += u64::pow(10, 3-i as u32) * digit as u64;
    }
    number
}

fn run_count_uniques(entries: &[Entry]) {
    let count = count_uniques(&entries);
    println!("Uniques: {}", count);
}

fn count_uniques(entries: &[Entry]) -> usize {
    let mut count = 0;
    for entry in entries {
        //print!("Uniques: ");
        count += entry.outputs.iter().filter(|x| match x.len() {
            2 | 3 | 4 | 7 => {
                //print!("{}, ", x);
                true
            }
            _ => false,
        }).count();
        //println!("");
    }
    count
}

fn read_input(input: &str) -> Result<Vec<Entry>> {
    let mut entries = Vec::new();
    for line in input.lines() {
        let parts: Vec<Vec<&str>> = line.split(" | ").map(|x| x.split(' ').collect()).collect();
        let mut patterns: Vec<String> = parts[0].iter().cloned().map(String::from).collect();
        let outputs = parts[1].iter().cloned().map(String::from).collect();
        patterns.sort_by(|a, b| a.len().cmp(&b.len()));
        entries.push(Entry { patterns, outputs });
    }
    Ok(entries)
}

fn main() {
    let content = std::fs::read_to_string("input").unwrap();
    //let content = std::fs::read_to_string("input-simple").unwrap();
    let entries = read_input(&content).unwrap();

    //run_count_uniques(&entries);
    let sum = run_brute_force_method(&entries).unwrap();
    println!("Sum is: {}", sum);
}

