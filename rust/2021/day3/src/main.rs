use std::fs;
use anyhow::{Result, bail};
use std::cmp::Ordering;

const NUM_BITS: usize = 12;
const INPUT_FILE: &str = "input";

fn main() {
    let (gamma, epsilon) = calculate_gamma_epsilon().unwrap();
    println!("Power consumption: {} (gamma: {1}, epsilon: {2}) (gamma: {:012b}, epsilon: {:012b})", gamma*epsilon, gamma, epsilon);

    let input = read_input_values().unwrap();
    println!("Calculate o2 generator rating");
    let o2_generator = calculate_life_support_value(&input, true).unwrap() as u64;
    println!("Calculate co2 scrubber rating");
    let co2_scrubber = calculate_life_support_value(&input, false).unwrap() as u64;
    println!("Life support rating: {}", o2_generator * co2_scrubber);
}

fn read_input_values() -> Result<Vec<u16>> {
    let mut values: Vec<u16> = Vec::new();
    let input = fs::read_to_string(INPUT_FILE)?;
    for line in input.lines().map(|x| x.trim()) {
        values.push(u16::from_str_radix(line, 2).unwrap());
    }
    Ok(values)
}

fn calculate_gamma_epsilon() -> Result<(u64, u64)> {
    let input = fs::read_to_string(INPUT_FILE)?;
    let total_count = input.lines().count();
    let mut gamma = 0;
    let mut epsilon = 0;

    for index in 0..NUM_BITS {
        let mask = 1 << ((NUM_BITS-1) - index);
        let mut ones = 0;
        for line in input.lines().map(|x| x.trim()) {
            ones += line.chars().nth(index).unwrap().to_digit(2).unwrap();
        }
        let zeros = total_count as u32 - ones;

        if ones >= zeros {
            gamma |= mask;
        } 
        if ones < zeros {
            epsilon |= mask;
        }
    }

    Ok((gamma, epsilon))
}

/// Returns
///   Equal if there's an equal number of 1's and 0's
///   Less if there's more zeros than ones
///   Greater if there's more ones than zeros
fn most_common_bit(values: &[u16], index: u8) -> Ordering {
    let mask: u16 = 1 << ((NUM_BITS-1) - (index as usize));
    let mut ones = 0;
    for v in values {
       if (v & mask) > 0 {
           ones += 1;
       }
    }
    let zeros = values.len() - ones;
    if ones > zeros {
        Ordering::Greater
    } else if ones < zeros {
        Ordering::Less
    } else {
        Ordering::Equal
    }
}

fn calculate_life_support_value(values: &[u16], prefer_common: bool) -> Result<u16> {
    let mut remaining = values.to_vec();
    let mut remove = Vec::new();

    if remaining.len() == 1 {
        return Ok(remaining[0]);
    }

    for index in 0..NUM_BITS {
        remove.clear();
        let shift = (NUM_BITS-1) - index;
        let mask = 1 << shift;
        let common_bit = match most_common_bit(&remaining, index as u8) {
            Ordering::Less => 0,
            Ordering::Greater | Ordering::Equal => 1,
        };
        println!("**** processing bit position {}, common_bit {}", index, common_bit);
        print!("**** [");
        for v in &remaining {
            print!("{:012b}, ", v);
        }
        println!("\n");
        for (i, v) in remaining.iter().enumerate() {
           let bit = (v & mask) >> shift;
           if (prefer_common && bit != common_bit) || (!prefer_common && (bit == common_bit)) {
              remove.push(i); 
           } 
        }
        // Remove indicies in reverse order to maintain correctness
        for idx in remove.iter().rev() {
            println!("**** removing {:012b} at index {}", remaining[*idx as usize], idx);
            remaining.remove(*idx as usize);
            if remaining.len() == 1 {
                println!("**** returning last value: {:012b} ({0})", remaining[0]);
                return Ok(remaining[0]);
            }
        }
    }
    if remaining.is_empty() {
        bail!("Empty!");
    } else {
        for v in &remaining {
            println!("**** multiple values at end: {:012b}", v);
        }
        Ok(remaining[0])
    }
}
