use anyhow::Result;
use std::fs;
use std::cmp;
use std::path::Path;

const INPUT_FILE: &str = "input";

#[derive(Debug, Clone, Copy, Default)]
struct Point {
    x: u16,
    y: u16,
    value: u8,
}

impl TryFrom<&str> for Point {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let fields: Vec<&str> = s.split(",").collect();
        if fields.len() != 2 {
            Err(format!("Invalid point entry in input: {:?}", fields))
        } else {
            let x = match fields[0].parse() {
                Ok(value) => value,
                Err(_e) => return Err(format!("Failed to parse point: {}", fields[0])),
            };
            let y = match fields[1].parse() {
                Ok(value) => value,
                Err(_e) => return Err(format!("Failed to parse point: {}", fields[1])),
            };
            Ok(Self { x, y, value: 0 })
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Line {
    p0: Point,
    p1: Point,
}

impl Line {
    fn is_diagonal(&self) -> bool {
        self.p0.x != self.p1.x && self.p0.y != self.p1.y
    }
}

impl TryFrom<&str> for Line {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let points: Vec<&str> = s.split(" -> ").collect();
        if points.len() != 2 {
            Err(format!("Invalid line entry in input: {}", s))
        } else {
            let p0 = points[0].try_into()?;
            let p1 = points[1].try_into()?;
            Ok(Self { p0, p1 })
        }
    }
}

struct Grid {
    rows: usize,
    cols: usize,
    points: Vec<Point>,
}

impl Grid {
    fn new(rows: usize, cols: usize) -> Self {
        Self {
            rows,
            cols,
            points: vec![Point::default(); rows * cols]
        }
    }

    fn render_lines(&mut self, lines: &[Line]) {
        for line in lines {
            self.render_line(line);
        }
    }

    fn render_line(&mut self, line: &Line) {
        // Horizontal line
        if line.p0.x == line.p1.x {
            let col = line.p0.x as usize;
            let start = cmp::min(line.p0.y, line.p1.y) as usize;
            let end = cmp::max(line.p0.y, line.p1.y) as usize;
            for row in start..=end {
                self.points[row*self.cols + col].value += 1;
            }
        // Vertical
        } else if line.p0.y == line.p1.y {
            let row = line.p0.y as usize;
            let start = cmp::min(line.p0.x, line.p1.x) as usize;
            let end = cmp::max(line.p0.x, line.p1.x) as usize;
            let offset = row*self.cols;
            for col in start..=end {
                self.points[offset+col].value += 1;
            }
        // Diagonal
        } else {
            let x0 = line.p0.x as i32;
            let x1 = line.p1.x as i32;
            let y0 = line.p0.y as i32;
            let y1 = line.p1.y as i32;
            let addx = if x1 > x0 { 1 } else { -1 };
            let addy = if y1 > y0 { 1 } else { -1 };
            let mut x = x0;
            let mut y = y0;
            while x != x1 && y != y1 {
                self.points[(y*(self.cols as i32) + x) as usize].value += 1;
                x += addx;
                y += addy;
            }
            self.points[(y*(self.cols as i32) + x) as usize].value += 1;
        }
    }

    fn print(&self) {
        for row in 0..self.rows {
            let offset = row*self.cols;
            println!("");
            for col in 0..self.cols {
                let index = offset + col;
                let value = self.points[index].value;
                if value > 0 {
                    print!("{:1}", value);
                } else {
                    print!(".");
                }
            }
        }
        println!("\n");
    }

    fn count_overlapping_points(&self) -> usize {
        let count: Vec<&Point> = self.points.iter().filter(|p| p.value > 1).collect();
        count.len()
    }
}


fn main() -> Result<()> {
    let contents = fs::read_to_string(INPUT_FILE)?;
    let lines = parse_input(contents.as_str())?;
    let (x, y) = calc_grid_size(&lines);
    let mut grid = Grid::new(x, y);
    grid.render_lines(&lines);
    grid.print();
    let count = grid.count_overlapping_points();
    println!("Overlapping points: {}", count);
    Ok(())
}

fn parse_input(content: &str) -> Result<Vec<Line>> {
    let mut lines: Vec<Line> = Vec::new();
    for entry in content.lines() {
        match entry.try_into() {
            Ok(l) => lines.push(l),
            Err(e) => eprintln!("{}", e),
        }
    }
    Ok(lines) 
}

fn calc_grid_size(lines: &Vec<Line>) -> (usize, usize) {
    let mut xmax = 0;
    let mut ymax = 0;
    for line in lines {
        let x = cmp::max(line.p0.x, line.p1.x);
        let y = cmp::max(line.p0.y, line.p1.y);
        if x > xmax { xmax = x; }
        if y > ymax { ymax = y; }
    }
    (xmax as usize + 1, ymax as usize + 1)
}

#[test]
fn test_grid() {
    let contents = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2";
    let lines = parse_input(contents).unwrap();
    let (x, y) = calc_grid_size(&lines);
    let mut grid = Grid::new(x, y);
    grid.render_lines(&lines);
    let count = grid.count_overlapping_points();
    assert!(count == 12);
}
