use anyhow::Result;
use std::collections::BTreeMap;

const DAYS_TO_SPAWN_NEW: u8 = 8;
const DAYS_TO_SPAWN_AGAIN: u8 = 6;

fn main() {
    //run_naive_computation();
    run_fast_computation();
}

#[allow(dead_code)]
fn run_fast_computation() {
    let mut fish: BTreeMap<u8, u64> = BTreeMap::new();
    let raw = read_input().unwrap();
    //let raw = vec![3,4,3,1,2];
    for f in raw {
        *fish.entry(f).or_default() += 1;
    }

    let days = 256;
    for _d in 0..days {
        //println!("----- day {} -----", _d);
        let mut newfish = 0;
        for f in 0..=DAYS_TO_SPAWN_NEW {
            let count = *fish.entry(f).or_default();
            //println!("bucket: {},  fish: {}", f, count);
            if f == 0 {
                newfish = count;
            } else {
                *fish.entry(f-1).or_default() = count;
                *fish.entry(f).or_default() = 0;
            }
        }
        // Add newly spawned fish
        *fish.entry(DAYS_TO_SPAWN_NEW).or_default() = newfish;
        // Reset existing fish back to SPAWN_AGAIN bucket
        *fish.entry(DAYS_TO_SPAWN_AGAIN).or_default() += newfish;
    }

    println!("There are {} lanternfish after {} days", fish.values().sum::<u64>(), days);
}

#[allow(dead_code)]
fn run_naive_computation() {
    let mut fish = read_input().unwrap();
    //let mut fish = vec![3,4,3,1,2];
    let days = 256;
    for _day in 0..days {
        elapse_day(&mut fish);
    }
    println!("There are {} lanternfish after {} days", fish.len(), days);
}

#[allow(dead_code)]
fn elapse_day(fish: &mut Vec<u8>) {
    let mut spawnlings = Vec::new();
    for f in fish.iter_mut() {
        if *f == 0 {
            spawnlings.push(DAYS_TO_SPAWN_NEW);
            *f = DAYS_TO_SPAWN_AGAIN;
        } else {
            *f -= 1;
        }
    } 
    fish.append(&mut spawnlings);
}

#[allow(dead_code)]
fn read_input() -> Result<Vec<u8>> {
    let contents = std::fs::read_to_string("input")?;
    let fish: Vec<u8> = contents.split(',').map(|x| x.trim().parse().unwrap()).collect();
    Ok(fish)
}
