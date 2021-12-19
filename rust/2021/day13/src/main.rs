use anyhow::{Result, bail};
use std::cmp;
use std::fs;
use std::collections::VecDeque;

#[derive(Debug)]
enum Fold {
    Horizontal(usize),
    Vertical(usize),
}

struct Grid {
    points: Vec<bool>,
    width: usize,
    height: usize,
    folds: VecDeque<Fold>,
}

impl Grid {
    fn render(&self) {
        for y in 0..self.height {
            for x in 0..self.width {
                if self.points[y * self.width + x] {
                    print!("{}", "#");
                } else {
                    print!("{}", ".");
                }
            }
            println!("");
        }
        println!("");
    }

    fn apply_next_fold(&mut self) -> Result<Fold> {
       match self.folds.pop_front() {
           Some(Fold::Horizontal(fold_y)) => {
              for y in fold_y+1..self.height {
                  for x in 0..self.width {
                     if self.get(x, y) {
                         let new_y = fold_coord(fold_y, y);
                         self.set(x, new_y, true);
                     }
                  }
              }  
              // Now shrink the grid to the new size
              self.height = fold_y;
              self.points.resize(self.width*self.height, false);
              Ok(Fold::Horizontal(fold_y))
           }
           Some(Fold::Vertical(fold_x)) => {
               let new_width = fold_x;
               let mut new_points = vec![false; new_width*self.height];
               for y in 0..self.height {
                   // Copy left half straight over
                   for x in 0..new_width {
                      if self.get(x, y) {
                          new_points[y*new_width+x] = true;
                      }
                   }
                   // Copy right half over
                   for x in new_width+1..self.width {
                       if self.get(x,y) {
                           let new_x = fold_coord(fold_x, x);
                           new_points[y*new_width + new_x] = true;
                       }
                   }
               }
               self.width = new_width;
               self.points = new_points;
               Ok(Fold::Vertical(fold_x))
           }
           _ => bail!("No more folds!"),
       }
    }

    fn dot_count(&self) -> usize {
        self.points.iter().filter(|p| **p).count()
    }


    fn get(&self, x: usize, y: usize) -> bool {
        self.points[y*self.width + x]
    }

    fn set(&mut self, x: usize, y: usize, val: bool) {
        self.points[y*self.width + x] = val;
    }
}

// Map a coordinate across a fold
fn fold_coord(fold: usize, val: usize) -> usize {
    fold - (val - fold)
}

fn main() {
    let mut grid = read_input().unwrap();
    grid.render();
    while let Ok(fold) = grid.apply_next_fold() {
        println!("Folding: {:?}", fold);
        grid.render();
        //println!("Dot Count: {}", grid.dot_count());
    } 
}

fn read_input() -> Result<Grid> {
    let contents = fs::read_to_string("input")?;
    //let contents = test_input();
    let mut coords = Vec::new();
    let mut folds = VecDeque::new();
    let mut width = 0;
    let mut height = 0;

    for line in contents.lines().filter(|l| !l.is_empty()).map(|l| l.trim()) {
        println!("{}", line);
        if line.starts_with("fold along y") {
            let y = line[13..].parse().unwrap();
            folds.push_back(Fold::Horizontal(y));
        } else if line.starts_with("fold along x") {
            let x = line[13..].parse().unwrap();
            folds.push_back(Fold::Vertical(x));
        } else {
            let p: Vec<usize> = line.split(',').map(|v| v.parse().unwrap()).collect();
            let x = p[0];
            let y = p[1];
            width = cmp::max(x + 1, width);
            height = cmp::max(y + 1, height);
            coords.push((x, y));
        }
    }

    let mut points = vec![false; width * height];
    for (x, y) in coords {
        points[y * width + x] = true;
    }

    Ok(Grid {
        points,
        width,
        height,
        folds,
    })
}

const fn test_input() -> &'static str {
r"6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"
}
