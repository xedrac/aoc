use std::fs::File;
use std::io::{BufReader, BufRead};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("input")?;
    let mut reader = BufReader::new(file);

    //println!("Increase count: {}", count_depth_increases(&mut reader));
    println!("Increase count(sum3): {}", count_boxcar_sum_increases(&mut reader, 3));
    Ok(())
}


fn count_depth_increases(reader: &mut BufReader<File>) -> u32 {
    let mut increase_count = 0;
    let mut last_depth = 0;
    for line in reader.lines().flatten() {
        let depth: u32 = line.parse().expect("invalid input");
        if last_depth != 0 && depth > last_depth {
            increase_count += 1;
        }
        last_depth = depth;
    }
    increase_count
}


fn count_boxcar_sum_increases(reader: &mut BufReader<File>, box_width: usize) -> u32 {
    let mut increase_count = 0;
    let mut last_sum = 0;
    let depths: Vec<usize> = reader.lines().flatten().map(|x| x.parse().expect("invalid input")).collect();
    let depths_len = depths.len();

    for (start, _) in depths.iter().enumerate() {
        let end = start + box_width;
        if end > depths_len {
            break;
        }
        let sum = &depths[start..end].iter().sum();
        println!("start: {}, end: {}, sum: {}", start, end, sum);
        if last_sum != 0 && *sum > last_sum {
            increase_count += 1;
        }
        last_sum = *sum;
    }
    increase_count
}
