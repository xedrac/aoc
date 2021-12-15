use std::fs::File;
use std::io::{self, BufReader, BufRead};
use std::path::Path;
use std::cmp;

pub fn sum_of_fuel_requirements(input: impl AsRef<Path>) -> io::Result<i64> {
    let f = File::open(input)?;
    let mut f = BufReader::new(f);
    let mut buf = String::with_capacity(32);
    let mut sum = 0i64;

    while let Ok(num_bytes) = f.read_line(&mut buf) {
        if num_bytes == 0 {
            println!("That was the last of em");
            break;
        } else {
            let value = buf.trim().parse::<i64>().unwrap();
            let fuel = fuel_requirement(value);
            sum += fuel;
            buf.clear();
        }
    }
    Ok(sum)
}

fn fuel_requirement(mass: i64) -> i64 {
    fn calc_fuel(m: i64) -> i64 {
        m / 3 - 2
    }
    fn added_fuel_requirement(fuel: i64) -> i64 {
        let added_fuel = calc_fuel(fuel);
        if added_fuel <= 0 {
            0
        } else {
            added_fuel + added_fuel_requirement(added_fuel)
        }
    }
    let base_fuel = calc_fuel(mass);
    let added_fuel = added_fuel_requirement(base_fuel);
    cmp::max(0, base_fuel + added_fuel)
}
