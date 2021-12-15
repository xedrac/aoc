mod day1;
mod day2;

const SRC_PATH: &str = "/home/sbadger/projects/aoc-2019/src/";

fn main() {
    let input = format!("{}/day1/input", SRC_PATH);
    let value = day1::sum_of_fuel_requirements(input).expect("failed to get a value");
    println!("Day1: {}", value);

    let input = format!("{}/day2/input2.1", SRC_PATH);
    let value = day2::run_program_from_1202_state(input).unwrap();
    print!("Day2: {}", value);
}
