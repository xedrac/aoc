use anyhow::Result;
use pathfinding::prelude::dijkstra;
use std::collections::HashMap;
use std::fs;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Point(usize, usize);
type Risk = usize;

struct Grid {
    points: Vec<Vec<Risk>>,
    adjacent: HashMap<Point, Vec<(Point, Risk)>>,
}

impl Grid {
    fn successors(&self, p: &Point) -> Vec<(Point, Risk)> {
        self.adjacent.get(p).unwrap().to_vec()
    }

    fn bottom_right(&self) -> Option<Point> {
        if self.points.is_empty() {
            None
        } else if self.points[0].is_empty() {
            None
        } else {
            Some(Point(self.points[0].len() - 1, self.points.len() - 1))
        }
    }

    fn find_shortest_path(&self, start: Point, end: Point) -> Risk {
        let result = dijkstra(&start, |p| self.successors(p), |p| *p == end);
        result.unwrap().1
    }

    fn expand_grid_part2(&mut self) {
        let width = self.points[0].len();
        let height = self.points.len();
        let new_width = width*5;
        let new_height = height*5;

        // Expand the grid by 5x
        self.points.resize(new_height, Vec::new());
        for y in 0..new_height {
            self.points[y].resize(new_width, 0);
        }
        // Populate the expanded parts as per part2 instructions
        /*
        // Start with populating the first tile in every new row
        for x in 0..width {
            for y in 0..height {
                for mult in 1..5 {
                    let tmp = (self.points[y][x] + mult) % 9;
                    let val = if tmp == 0 { 9 } else { tmp };  // Wrap back to 1, not 0
                    self.points[mult*height+y][width] = val;
                }
            }
        }
        */

        // Now expand each first row tile to all columns to the right of it 
        for ymult in 0..5 {
            for y in 0..height {
                for xmult in 0..5 {
                    for x in 0..width {
                        let tmp = (self.points[y][x] + xmult + ymult) % 9;
                        let val = if tmp == 0 { 9 } else { tmp };  // Wrap back to 1, not 0
                        self.points[ymult*height+y][xmult*width+x] = val;
                    }
                }
            }
        }

        self.update_successors();
    }

    fn update_successors(&mut self) {
        let height = self.points.len();
        let width = self.points[0].len();
        for y in 0..height {
            for x in 0..width {
                let mut v = Vec::new();
                if x > 0 {
                    v.push((Point(x - 1, y), self.points[y][x - 1]));
                }
                if x < width - 1 {
                    v.push((Point(x + 1, y), self.points[y][x + 1]));
                }
                if y > 0 {
                    v.push((Point(x, y - 1), self.points[y - 1][x]));
                }
                if y < height - 1 {
                    v.push((Point(x, y + 1), self.points[y + 1][x]));
                }
                self.adjacent.insert(Point(x, y), v);
            }
        }
    }

    fn print_points(&self) {
        for (y, row) in self.points.iter().enumerate() {
            if y % 10 == 0 {
                println!("");
            }
            println!("");
            for (x, risk) in row.iter().enumerate() {
                if x % 10 == 0 {
                    print!(" ");
                }
                print!("{}", risk);
            }
        }
        println!("");
    }
}

fn main() {
    let mut grid = read_input().unwrap();
    let start = Point(0, 0);
    let end = grid.bottom_right().unwrap();
    let risk = grid.find_shortest_path(start, end);
    //println!("{:?}", grid.points);
    //println!("");
    println!("Risk: {}", risk);

    grid.expand_grid_part2();
    //println!("{:?}", grid.points);
    grid.print_points();
    let start = Point(0, 0);
    let end = grid.bottom_right().unwrap();
    let risk = grid.find_shortest_path(start, end);
    println!("Risk2: {}", risk);
}

fn read_input() -> Result<Grid> {
    let content = fs::read_to_string("input")?;
    //let content = fs::read_to_string("input-simple")?;
    let mut points = Vec::new();
    for line in content.lines() {
        points.push(Vec::new());
        let pline = &mut points.last_mut().unwrap();
        for risk in line.chars().map(|c| c.to_digit(10).unwrap() as usize) {
            pline.push(risk);
        }
    }
    let mut grid = Grid { points, adjacent: HashMap::new() };
    grid.update_successors();
    Ok(grid)
}
