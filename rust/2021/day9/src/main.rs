use anyhow::Result;
use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
struct Point(i32, i32);

struct Grid {
    points: Vec<Vec<u8>>,
    marked: HashMap<Point, bool>,
}

fn main() {
    let mut grid = read_input().unwrap();
    //for row in &grid.points {
    //    println!("{:?}", row);
    //}
    let low_points = get_low_points(&grid);
    let risk: u64 = low_points.iter().map(|p| grid.points[p.1 as usize][p.0 as usize] as u64 + 1).sum();
    println!("Low Points: {:?}", low_points);
    println!("Risk: {}", risk);

    let mut basins = Vec::new();
    for low in &low_points {
        let basin = find_basin_points(&mut grid, low);
        basins.push(basin.len() as i32);
        //if basin.len() > 0 {
        //    println!("Found basin of len={}", basin.len());
        //}
    }

    basins.sort();
    let largest3_product: i32 = basins.iter().rev().take(3).product();
    println!("Largest 3 basins product: {}", largest3_product);
}

fn get_low_points(grid: &Grid) -> Vec<Point> {
    let mut low_points = Vec::new();
    for y in 0..grid.points.len() {
        for x in 0..grid.points[0].len() {
            if is_low_point(&grid, x as i32, y as i32) {
                low_points.push(Point(x as i32, y as i32));
            }
        }
    }
    low_points
}

fn is_low_point(grid: &Grid, x: i32, y: i32) -> bool {
    let value = grid.points[y as usize][x as usize];
    let adjacent = vec![Point(x, y-1), Point(x-1, y), Point(x+1, y), Point(x, y+1)];
    adjacent.iter().filter(|p| get_point(&grid, p.0, p.1).is_some()).all(|p| value < grid.points[p.1 as usize][p.0 as usize])
}

fn get_point(grid: &Grid, x: i32, y: i32) -> Option<u8> {
    if let Some(row) = grid.points.get(y as usize) {
        if let Some(value) = row.get(x as usize) {
            return Some(*value);
        }
    }
    None 
}

fn get_adjacent_points(grid: &Grid, point: &Point) -> VecDeque<Point> {
    let x = point.0;
    let y = point.1;
    let adj = [(x-1, y), (x, y-1), (x+1, y), (x, y+1)];
    adj.iter().filter(|p| get_point(&grid, p.0, p.1).is_some()).map(|p| Point(p.0, p.1)).collect()
}

fn is_marked(grid: &Grid, point: &Point) -> bool {
    *grid.marked.get(&point).unwrap()
}

fn mark(grid: &mut Grid, point: &Point) {
    *grid.marked.get_mut(&point).unwrap() = true;
}

// Given a starting point (presumably in a basin), find all the points
// that are part of that basin.  We accomplish this by looking at all
// adjacent points that are not marked, and pushing them onto a queue.
// We then repeat for each item on the queue until the queue is empty.
fn find_basin_points(grid: &mut Grid, start: &Point) -> Vec<Point> {
    let mut basin = Vec::new();
    let mut queue = VecDeque::new();

    if !is_marked(grid, start) {
        mark(grid, start);
        queue.push_back(*start);
    }
    while let Some(p) = queue.pop_front() {
        let adj_points = get_adjacent_points(&grid, &p);
        basin.push(p);
        // Now push all unmarked adjacent points onto the queue
        for adj in &adj_points {
            if !is_marked(grid, adj) {
                mark(grid, adj);
                queue.push_back(*adj);
            }
        }
    }
    basin
}


fn read_input() -> Result<Grid> {
    let contents = std::fs::read_to_string("input")?;
    //let contents = std::fs::read_to_string("input-small")?;
    let mut grid = Grid { points: Vec::new(), marked: HashMap::new() };

    for (y, line) in contents.lines().enumerate() {
        let row = line.chars().map(|c| c.to_digit(10).unwrap() as u8).collect::<Vec<u8>>(); 
        for (x, value) in row.iter().enumerate() {
            // Mark all 9's so we can easily test for basins
            let marked = grid.marked.entry(Point(x as i32, y as i32)).or_insert(false);
            if *value == 9 {
                *marked = true;
            } 
        }
        grid.points.push(row);
    }

    Ok(grid)
}
