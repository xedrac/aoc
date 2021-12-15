use std::fs;

/// Implements an iterator that goes spans all possible 3-tuple combinations
/// For example, given the vec [0, 1, 2], calls to next() will return, in order:
/// (0, 0, 0) (0, 0, 1) (0, 0, 2) (0, 1, 0) (0, 1, 1) (0, 1, 2) (0, 2, 0) ...
#[derive(Default)]
struct TriPermutation {
    values: Vec<i32>,
    index0: usize, // outer loop index
    index1: usize, // inner loop index
    index2: usize, // inner-inner loop index
}

impl Iterator for TriPermutation {
    type Item = (i32, i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.values.len();
        // When index2 hits the end of the list, wrap it  back to 0 and increment index1
        if self.index2 >= len {
            self.index2 = 0;
            self.index1 += 1;
        }
        // When index1 hits the end of the list, wrap it back to 0 and increment index0
        if self.index1 >= len {
            self.index1 = 0;
            self.index0 += 1;
        }
        // If index0 has hit the end of the list, we're done
        if self.index0 >= len {
            None
        } else {
            self.index2 += 1;
            Some((self.values[self.index0], self.values[self.index1], self.values[self.index2-1]))
        }
    }
}

impl TriPermutation {
    fn new(values: Vec<i32>) -> Self {
        Self {
            values,
            ..Default::default()
        }
    }
}

pub fn run() {
    let values = get_sorted_input();

    let perms = TriPermutation::new(values);
    for (a, b,c) in perms {
        if a + b + c == 2020 {
            println!("part2: {} + {} + {} = 2020", a, b, c);
            println!("part2: {} * {} * {} = {}", a, b, c, a*b*c);
            return;
        }
    }

    println!("part2: no 3-tuple found that adds to 2020!");
}


fn get_sorted_input() -> Vec<i32> {
    let contents = fs::read_to_string("input/part1").unwrap();
    let mut values = vec![];

    for line in contents.lines() {
        if let Ok(v) = line.parse::<i32>() {
            values.push(v);
        }
    }
    values.sort_unstable();
    values
}