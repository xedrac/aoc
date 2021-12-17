use std::fs;
use std::collections::VecDeque;

fn main() {
    let content = fs::read_to_string("input").unwrap();
    let lines: Vec<Vec<char>> = content.lines().map(|s| s.chars().collect()).collect();

    let score = score_invalid_lines(&lines);
    println!("Invalid Lines Score: {}", score);

    let score2 = score_incomplete_lines(&lines);
    println!("Incomplete Lines Score: {}", score2);
}

fn score_incomplete_lines(lines: &[Vec<char>]) -> u64 {
    let mut scores = Vec::new();
    for line in lines {
        if find_first_invalid_char(&line).is_some() {
            continue;  // skip invalid lines
        }
        let score = score_incomplete_line(&line);
        scores.push(score);
    }
    scores.sort();
    let index = scores.len() / 2;
    scores[index]
}

fn score_incomplete_line(line: &[char]) -> u64 {
    let mut score = 0u64;
    let ending = find_missing_ending(line);
    //println!("Missing ending: {:?}", ending);
    for c in ending {
        score = score * 5;
        score += match c {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => 0,
        };
    }
    score
}

fn find_missing_ending(line: &[char]) -> Vec<char> {
    let mut stack = VecDeque::new();
    for c in line {
        match c {
            '(' | '[' | '{' | '<' => stack.push_front(c),
            _ => { stack.pop_front(); }
        }
    }
    stack.into_iter().map(|c| close_pair(*c).unwrap()).collect()
}

fn score_invalid_lines(lines: &[Vec<char>]) -> u64 {
    let mut score = 0;
    for line in lines {
        if let Some(c) = find_first_invalid_char(line) {
            score += match c {
                ')' => 3,
                ']' => 57,
                '}' => 1197,
                '>' => 25137,
                _ => 0,
            }
        }
    } 
    score
}

fn find_first_invalid_char(line: &[char]) -> Option<char> {
    let mut stack = VecDeque::new();
    for c in line {
        match c {
            '(' | '[' | '{' | '<' => stack.push_front(c),
            _ => {
                let open = stack.pop_front().unwrap_or(&' ');
                if !is_matched(*open, *c) {
                    return Some(*c);
                }
            }
        }
    }
    None
}

fn close_pair(open: char) -> Option<char> {
    match open {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        '<' => Some('>'),
        _ => None,
    }
}

fn is_matched(open: char, close: char) -> bool {
    if let Some(c) = close_pair(open) {
        c == close
    } else {
        false
    }
}


