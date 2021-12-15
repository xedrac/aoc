use std::fs;
use std::io;
use std::path::Path;
use anyhow::{anyhow, Result};

pub fn parse_program(path: impl AsRef<Path>) -> io::Result<Vec<u32>> {
    let v = fs::read_to_string(path)?.trim().split(",").map(|x| x.parse::<u32>().unwrap()).collect();
    Ok(v)
}

pub fn run_program(data: &mut [u32]) -> Result<u32> {
    for x in (0..data.len()).step_by(4) {
        if data[x] == 99 {
            return Ok(data[0]);
        }

        let p0 = data[data[x+1] as usize];
        let p1 = data[data[x+2] as usize];
        let rindex = data[x+3] as usize;

        // Addition
        if data[x] == 1 {
            data[rindex] = p0 + p1;
        } else if data[x] == 2 {
            data[rindex] = p0 * p1;
        }
    }
    Err(anyhow!("Bad input"))
}

pub fn run_program_from_1202_state(path: impl AsRef<Path>) -> Result<u32> {
    let mut vec = parse_program(path)?;
    vec[1] = 12;
    vec[2] = 2;
    run_program(&mut vec)
}