use anyhow::Result;

fn main() {
    let mut values = read_input().unwrap();
    values.sort();
    println!("SIMPLE fuel: {}", calc_fuel_basic(&values));
    println!("COMPLEX fuel: {}", calc_fuel_complex(&values));

}

fn calc_fuel_complex(values: &[i64]) -> i64 {
    let avg = values.iter().sum::<i64>() / values.len() as i64;
    println!("average is: {} (found: {})", avg, values.contains(&avg));

    let mut fuel = 0;
    for x in values {
        let n = (x - avg).abs();
        fuel += n*(n+1) / 2;
    }
    fuel
}

fn calc_fuel_basic(values: &[i64]) -> i64 {
    let index = values.len() / 2;
    let median = values[index];
    //println!("median is: {}", median);
    let mut fuel = 0;
    for x in values {
        fuel += (x - median).abs();
    }
    fuel
}


fn read_input() -> Result<Vec<i64>> {
    let content = std::fs::read_to_string("input")?;
    let values = content.split(',').map(|x| x.trim().parse().unwrap()).collect();
    Ok(values)
}      
