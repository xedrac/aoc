use std::fs;

/// Implements an iterator that goes spans all possible 2-tuple combinations
/// For example, given the vec [0, 1, 2], calls to next() will return, in order:
/// (0, 0) (0, 1) (0, 2) (1, 0) (1, 1) (1, 2) (2, 0) (2, 1) (2, 2)
#[derive(Default)]
struct PairPermutation {
    values: Vec<i32>,
    index0: usize, // outer loop index
    index1: usize, // inner loop index
}

impl Iterator for PairPermutation {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.values.len();
        // When index1 hits the end of the list, wrap it back to 0 and increment index0
        if self.index1 >= len {
            self.index1 = 0;
            self.index0 += 1;
        }
        // If index0 has hit the end of the list, we're done
        if self.index0 >= len {
            None
        } else {
            self.index1 += 1;
            Some((self.values[self.index0], self.values[self.index1-1]))
        }
    }
}

impl PairPermutation {
    fn new(values: Vec<i32>) -> Self {
        Self {
            values,
            ..Default::default()
        }
    }
}

pub fn run() {
    let values = get_sorted_input();

    let perms = PairPermutation::new(values);
    for (a, b) in perms {
        if a + b == 2020 {
            println!("part1: {} + {} = 2020", a, b);
            println!("part1: {} * {} = {}", a, b, a*b);
            return;
        }
    }

    println!("part1: no pair found that adds to 2020!");
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