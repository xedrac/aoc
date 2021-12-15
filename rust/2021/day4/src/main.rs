use anyhow::Result;
use std::fmt;
use std::collections::BTreeMap;

const BOARD_SIZE: usize = 5;
const INPUT_FILE: &str = "input";

#[derive(Debug, Default, Clone)]
struct Square {
    value: u8,
    marked: bool,
}

#[derive(Debug, Default, Clone)]
struct Board {
    squares: Vec<Vec<Square>>,
}

impl Board {
    fn new() -> Self {
        Self {
            squares: vec![vec![Square::default(); BOARD_SIZE]; BOARD_SIZE],
        }
    }
}

impl From<&str> for Board {
    fn from(s: &str) -> Self {
       let mut board = Board::new();
       for (y, line) in s.lines().map(|l| l.trim()).enumerate() {
           for (x, v) in line.replace("  ", " ").split(" ").map(|n| n.parse::<u8>().unwrap()).enumerate() {
               board.squares[y][x].value = v;
           }
       }
       board
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.squares {
            for square in row {
                let _ = write!(f, "{:02}{} ", square.value, if square.marked {"*"} else {" "});
            }
            let _ = writeln!(f, "");
        }
        writeln!(f, "")
    }
}

fn main() {
    let (numbers, mut boards) = read_input().unwrap();
    print_winning_board(boards.clone(), &numbers);
    print_losing_board(&mut boards, &numbers);
}

fn print_winning_board(mut boards: Vec<Board>, numbers: &[u8]) {
    for number in numbers {
        //println!("Called Number: {}", number);
        for mut board in &mut boards {
            if call_number(&mut board, *number) {
                println!("\nBINGO: (score={})\n{}", board_score(&board, *number), board);
                return;
            }
        }
    }
}

fn bingo_turn_count(board: &mut Board, numbers: &[u8]) -> (u32, u8) {
    let mut turns = 0;
    for number in numbers {
        turns += 1;
        if call_number(board, *number) {
            return (turns, *number);
        }
    }
    unreachable!("Did not reach bingo for: \n{}\n", board);
}

fn print_losing_board(boards: &mut Vec<Board>, numbers: &[u8]) {
    let mut turns = BTreeMap::new();
    for board in boards {
        let (turn_count, last_number) = bingo_turn_count(board, numbers);
        turns.insert(turn_count, (board.clone(), last_number)); 
    }

    let worst = turns.keys().max().unwrap();
    let (board, number) = turns.get(worst).unwrap();
    println!("\nWORST_BINGO: (score={})\n{}", board_score(&board, *number), board);
}

fn read_input() -> Result<(Vec<u8>, Vec<Board>)> {
    let input = std::fs::read_to_string(INPUT_FILE)?; 
    let numbers_line = input.lines().nth(0).unwrap();
    let numbers = numbers_line.split(",").map(|x| x.parse::<u8>().unwrap()).collect();
    let mut boards = Vec::new();

    for b in input.split("\n\n").skip(1) {
        boards.push(b.into());
        //println!("{:?}", b); 
        //println!("");
    } 
    Ok((numbers, boards))
}

fn call_number(board: &mut Board, number: u8) -> bool {
    for row in &mut board.squares {
        for mut square in row {
            if square.value == number {
                square.marked = true;
            }
        }
    }
    has_bingo(board)
}


fn has_bingo(board: &Board) -> bool {
    // Check rows for bingo
    for row in &board.squares {
        if row.iter().all(|x| x.marked) {
            return true;
        }
    }
    // Check colums for bingo
    for x in 0..BOARD_SIZE {
        let mut bingo = true;
        for row in &board.squares {
            if !row[x].marked {
                bingo = false;
                break;
            } 
        }
        if bingo == true {
            return true;
        }
    }
    false
}

fn board_score(board: &Board, last_number: u8) -> u32 {
    let mut sum: u32 = 0;
    for row in &board.squares {
        for square in row {
            if !square.marked {
                sum += square.value as u32;
            }
        }
    }    
    sum * (last_number as u32)
}
