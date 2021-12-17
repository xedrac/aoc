use std::fs;
use std::fmt;
use anyhow::Result;
use std::collections::HashMap;

#[derive(Debug)]
struct Grid {
    energy: Vec<Vec<u8>>,
}

impl Grid {
    fn neighbors(&self, col: usize, row: usize) -> Vec<(usize, usize)> {
        let x = col as i32;
        let y = row as i32;
        let adj = [(x, y-1), (x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1)];
        adj.iter().filter(|p| self.is_valid(p.0, p.1)).map(|p| (p.0 as usize, p.1 as usize)).collect()
    }

    fn is_valid(&self, x: i32, y: i32) -> bool {
        let nx = self.energy[0].len() as i32;
        let ny = self.energy.len() as i32;
        x >= 0 && x < nx && y >= 0 && y < ny
    }

    fn flash(&mut self, col: usize, row: usize) {
        //self.energy[row][col] = 0; 
        let adj = self.neighbors(col, row);
        for p in adj {
            self.energy[p.1][p.0] += 1;
        }
    }

    fn increment_all(&mut self) {
        for row in &mut self.energy {
            for energy in row {
                *energy += 1;
            }
        }
    }

    fn get(&self, col: usize, row: usize) -> u8 {
        self.energy[row][col]
    }

    fn set(&mut self, col: usize, row: usize, value: u8) {
        self.energy[row][col] = value; 
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.energy {
            for energy in row { 
                let _ = write!(f, "{}", energy);
            }
            let _ = writeln!(f, "");
        }
        writeln!(f, "")
    }
}

fn main() {
    let mut grid = read_input().unwrap();
    let iterations = 210;

    let mut total_flashes = 0;
    for i in 0..iterations {
        //println!("Iteration: {}", i);
        //println!("-------------");
        //println!("{}", grid);
        let (flashes, all_flashed) = iterate(&mut grid);
        total_flashes += flashes;
        if all_flashed {
            println!("Synchronized flash on step: {}", i+1);
            println!("{}", grid);
        }
    }
    println!("Flashes after {} iterations: {}", iterations, total_flashes);
}

// Returns the number of flashes that occurred
fn iterate(grid: &mut Grid) -> (u32, bool) {
    let nx = grid.energy[0].len();
    let ny = grid.energy.len();
    let mut flashed: HashMap<(usize, usize), bool> = HashMap::new();
    let mut flash_count = 0;
    let mut something_flashed = true;
    
    grid.increment_all();
    
    while something_flashed {
        something_flashed = false;
        for x in 0..nx {
            for y in 0..ny {
                let already_flashed = *flashed.entry((x, y)).or_default();
                if grid.get(x, y) > 9 && !already_flashed {
                    grid.flash(x, y);
                    *flashed.entry((x, y)).or_default() = true;
                    flash_count += 1;
                    something_flashed = true;
                }
            }
        }
    }

    // Set all points that flash to energy 0
    for key in flashed.keys() {
        if *flashed.get(key).unwrap() {
            grid.set(key.0, key.1, 0);
        }
    }

    let all_flashed = flashed.values().into_iter().all(|v| *v);

    (flash_count, all_flashed)
}

fn read_input() -> Result<Grid> {
    let contents = fs::read_to_string("input")?;
    //let contents = fs::read_to_string("input-small")?;
    let mut grid = Grid { energy: Vec::new() };
    for line in contents.lines() {
        grid.energy.push(line.chars().map(|c| c.to_digit(10).unwrap() as u8).collect()); 
    }
    Ok(grid)
}

