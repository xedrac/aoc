use std::collections::BTreeMap;
use std::fs;
use std::fmt;
use anyhow::Result;
use std::cmp::Ordering;
use std::cell::RefCell;
use std::rc::Rc;
//use std::borrow::{Borrow, BorrowMut};


#[derive(Debug)]
struct Cave {
    name: String,
    orphan: bool,
    adjacent: Vec<Rc<RefCell<Cave>>>,
}

impl Cave {
    fn new(name: String) -> Self {
        //Self { name, visited: false, orphan: false, adjacent: Vec::new() }
        Self { name, orphan: false, adjacent: Vec::new() }
    }

    //fn visitable(&self) -> bool {
    //    self.is_end() || (!self.orphan && !self.is_start() && (!self.visited || self.revisitable()))
    //}

    fn revisitable(&self) -> bool {
        self.name == self.name.to_uppercase()
    }

    fn is_start(&self) -> bool {
        self.name == "start"
    }

    fn is_end(&self) -> bool {
        self.name == "end"
    }
}

impl PartialEq for Cave {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Cave {}

impl Ord for Cave {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Cave {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct Graph {
    caves: BTreeMap<String, Rc<RefCell<Cave>>>,
    paths: Vec<String>,
}

impl Graph {
    fn new() -> Self {
        Self { caves: BTreeMap::new(), paths: Vec::new() }
    }   

    fn find_paths(&mut self) {
        let mut stack = Vec::new();         
        let start = "start";
        stack.push((start.to_string(), vec![], "".to_string()));

        while let Some((name, mut prefix, twice)) = stack.pop() {
            //println!("Pop: ({}, {})   [stack: {}]", name, prefix.join(","), stack.iter().map(|x| x.0.clone()).collect::<Vec<String>>().join(","));
            let cave = self.caves.get_mut(&name).unwrap().borrow_mut();
            prefix.push(name);
            if cave.is_end() {
                self.paths.push(prefix.join(",").to_string());
                continue;
            } else {
                //if !cave.adjacent.is_empty() {
                //    print!("Push: ");
                //}
                for adj in &cave.adjacent {
                    let name = &adj.borrow().name;
                    let visitable = adj.borrow().revisitable() || !prefix.contains(&name) || twice.is_empty();
                    if visitable && !adj.borrow().is_start() {
                        let mut newtwice = twice.clone();
                        if !adj.borrow().revisitable() && prefix.contains(&name) && twice.is_empty() {
                            newtwice = name.clone();
                        }
                        //print!("({}, {}) ", adj.borrow().name, prefix.join(","));
                        stack.push((adj.borrow().name.clone(), prefix.clone(), newtwice)); 
                    }
                }
                //println!("");
            }
        }
    }
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, cave) in self.caves.iter() {
            let c = cave.borrow();
            let _ = write!(f, "{}{} - [", name, if c.orphan { "(orphan)" } else { "" });
            for (i, v) in c.adjacent.iter().enumerate() {
                if i > 0 {
                    let _ = write!(f, ", ");
                }
                let _ = write!(f, "{}", v.borrow().name);
            }
            let _ = writeln!(f, "]");
        }
        Ok(())
    }
}

fn main() {
    let mut graph = read_input().unwrap();
    //println!("{}", graph); 
    graph.find_paths();

    for p in &graph.paths {
        println!("{}", &p);
    }
    println!("Path Count: {}", graph.paths.len());
}

fn read_input() -> Result<Graph> {
    let contents = fs::read_to_string("input")?;
    //let contents = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end".to_string();
    let mut graph = Graph::new();
    for entry in contents.lines().map(|l| l.split('-').collect::<Vec<_>>()) {
        let n0 = entry[0];
        let n1 = entry[1];
        if !graph.caves.contains_key(n0) {
            let c0 = Rc::new(RefCell::new(Cave::new(n0.to_string())));
            graph.caves.insert(n0.to_string(), c0);
        }
        if !graph.caves.contains_key(n1) {
            let c1 = Rc::new(RefCell::new(Cave::new(n1.to_string())));
            graph.caves.insert(n1.to_string(), c1);
        }
        let c0 = graph.caves.get(n0).unwrap();
        let c1 = graph.caves.get(n1).unwrap();
        if !c0.borrow().adjacent.contains(c1) {
            c0.borrow_mut().adjacent.push(c1.clone());
        }
        if !c1.borrow().adjacent.contains(c0) {
            c1.borrow_mut().adjacent.push(c0.clone());
        }
    }

    // Tag orphaned caves
    for (_name, cave) in &mut graph.caves {
        let mut c = cave.borrow_mut();
        if c.adjacent.len() == 1 && !c.adjacent[0].borrow().revisitable() && !c.adjacent[0].borrow().is_end() {
            c.orphan = true;
        }
    }
    Ok(graph)
}




/*
use std::collections::BTreeMap;
use std::fs;
use std::fmt;
use anyhow::Result;
use pathfinding::prelude::dfs;
use std::cmp::Ordering;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
struct Cave {
    name: String,
    large: bool,
    visited: bool,
}

impl Cave {
    fn new(name: String, large: bool) -> Self {
        Self { name, large, visited: false }
    }

    fn revisitable(&self) -> bool {
        self.name == self.name.to_uppercase()
    }

    fn is_end(&self) -> bool {
        self.name == "end"
    }
}

impl PartialEq for Cave {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Cave {}

impl Ord for Cave {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Cave {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct Graph {
    caves: BTreeMap<String, Vec<Rc<RefCell<Cave>>>>,
    start: Option<Rc<RefCell<Cave>>>,
    end: Option<Rc<RefCell<Cave>>>,
    orphans: BTreeMap<String, Rc<RefCell<Cave>>>,
    paths: BTreeMap<String, bool>,
}

impl Graph {
    fn new() -> Self {
        Self { caves: BTreeMap::new(), start: None, end: None, orphans: BTreeMap::new(), paths: BTreeMap::new() }
    }   

    fn find_paths(&mut self) {
        let start = self.start.clone().unwrap();
        let history = Rc::new(RefCell::new(Vec::new()));

        while let Some(path) = dfs(&start, |n| {
            let name = n.borrow().name.clone();
            //println!("{},", &name);
            n.borrow_mut().visited = true;
            history.borrow_mut().push(n.clone());
            self.caves.get(&name).unwrap().iter().filter(|c| {
                if c.borrow().large {
                    true
                } else if !c.borrow().visited && !self.orphans.contains_key(&name) {
                    true
                } else {
                    false
                }
            }).collect::<Vec<&Rc<RefCell<Cave>>>>()
        }, |n| {
            let p = format_path(&history.borrow());
            if n.borrow().is_end() && !self.paths.contains_key(&p) {
                self.paths.insert(p, true);
                true
            } else {
                false
            }
        }) {
            let pathstr = format_path(&path);
            println!("{}", &pathstr);
            self.paths.insert(pathstr, true);
        }
    }
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (key, val) in self.caves.iter() {
            let _ = write!(f, "{} - [", key);
            for (i, v) in val.iter().enumerate() {
                if i > 0 {
                    let _ = write!(f, ", ");
                }
                let _ = write!(f, "{}", v.borrow().name);
            }
            let orphaned = self.orphans.contains_key(key);
            let _ = writeln!(f, "] {}", if orphaned { "(orphaned)" } else { "" });
        }
        Ok(())
    }
}

fn main() {
    let mut graph = read_input().unwrap();
    //println!("{}", graph); 
    graph.find_paths();
}

fn read_input() -> Result<Graph> {
    //let contents = fs::read_to_string("input")?;
    let contents = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end".to_string();
    let mut graph = Graph::new();
    for entry in contents.lines().map(|l| l.split('-').collect::<Vec<_>>()) {
        let n0 = entry[0];
        let n1 = entry[1];
        let n0_large = n0 == n0.to_ascii_uppercase();
        let n1_large = n1 == n1.to_ascii_uppercase();
        if !graph.caves.contains_key(n0) {
            graph.caves.insert(n0.to_string(), Vec::new());
        }
        if !graph.caves.contains_key(n1) {
            graph.caves.insert(n1.to_string(), Vec::new());
        }
        let c0 = Rc::new(RefCell::new(Cave::new(n0.to_string(), n0_large)));
        let c1 = Rc::new(RefCell::new(Cave::new(n1.to_string(), n1_large)));
        if n0 == "start" {
            graph.start = Some(c0.clone());
        } else if n1 == "start" {
            graph.start = Some(c1.clone());
        }
        if n0 == "end" {
            graph.end = Some(c0.clone());
        } else if n1 == "end" {
            graph.end = Some(c1.clone());
        }
        graph.caves.get_mut(n0).unwrap().push(c1);
        graph.caves.get_mut(n1).unwrap().push(c0);
    }

    // Tag orphaned caves
    for (key, values) in &mut graph.caves {
        if values.len() == 1 && !values.first().unwrap().borrow().revisitable() {
            graph.orphans.insert(key.to_string(), values.first().unwrap().clone());
        }
    }
    Ok(graph)
}

fn format_path(path: &[&Rc<RefCell<Cave>>]) -> String {
    let mut pathstr = String::new();
    for (i, c) in path.iter().enumerate() {
        if i > 0 {
            pathstr += ",";
        }
        pathstr += &c.borrow().name;
    }
    pathstr
}

*/
