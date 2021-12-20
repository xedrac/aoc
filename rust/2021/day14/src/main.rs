use anyhow::Result;
use std::fs;
use std::collections::HashMap;
use rayon::prelude::*; 
use std::sync::Mutex;

const CACHE_ITERS: usize = 10;

#[derive(Debug, Clone)]
struct Stats {
    most: char,
    most_len: usize,
    least: char,
    least_len: usize,
    all: HashMap<char, usize>,
}

/*
impl<T: Send> FromParallelIterator<T> for Stats {
    fn from_par_iter<I>(par_iter: I) -> Self
        where I: IntoParallelIterator<Item = T>
    {
        let par_iter = par_iter.into_par_iter();
        Self {

        }
    }   
}
*/

impl Stats {
    fn new() -> Self {
        Self {
            most: '?',
            most_len: 0,
            least: '?',
            least_len: 0,
            all: HashMap::new(),
        }
    }
    fn combine(&mut self, other: &Self) {
        for (key, val) in other.all.iter() {
            if self.all.contains_key(key) {
                *self.all.get_mut(key).unwrap() += val;
            } else {
                self.all.insert(*key, *val);
            }
        }
    }
    fn update(&mut self) {
        let most = self.all.iter().max_by_key(|entry| entry.1).unwrap();
        let least = self.all.iter().min_by_key(|entry| entry.1).unwrap();
        self.most = *most.0;
        self.most_len = *most.1;
        self.least = *least.0;
        self.least_len = *least.1;
    }
}

fn main() {
    let (template, rules) = read_input().unwrap();
    let cache = build_cache(&rules, CACHE_ITERS);
    println!("Cache: {:?}", cache);
    let stats = score_template(&template, &cache);
    println!("Stats: {:?}", stats);
    println!("Score (iters={}): {}", CACHE_ITERS*4, stats.most_len - stats.least_len);
    
    /*
    let stats = element_stats(&polymer);
    let most = stats.first().unwrap();
    let least = stats.last().unwrap();
    println!("Most common element: {} ({} instances)", most.0, most.1);
    println!("Least common element: {} ({} instances)", least.0, least.1);
    println!("Difference: {}", most.1 - least.1);
    */
}

fn score_template(template: &str, cache: &HashMap<String, (String, Stats)>) -> Stats {
    //let all_stats = Mutex::new(HashMap::new());
    let mut keys = Vec::new();
    for i in 0..template.len()-1 {
        keys.push(template[i..i+2].to_string());
    }

    let mut local_stats = Vec::new();
        
    keys.par_iter().map(|key0| {
    //for i in 0..template.len()-1 {
    //    let key0 = &template[i..i+2];
        let mut local_stats = Stats::new();
        let (value0, _stats0) = cache.get(key0).unwrap();
        println!("\nProcessing key0: {} -> {}", key0, &value0);
        for k in 0..value0.len()-1 {
            let key1 = &value0[k..k+2];
            let (value1, _stats1) = cache.get(key1).unwrap();  
            //println!("Processing key1: {} -> {}", key1, &value1);
            for j in 0..value1.len()-1 {
                let key2 = &value1[j..j+2];
                let (value2, _stats2) = cache.get(key2).unwrap();
                //println!("Processing key2: {} -> {}", key2, &value2);
                for m in 0..value2.len()-1 {
                    let key3 = &value2[m..m+2];
                    let (_value3, stats3) = cache.get(key3).unwrap();
                    //println!("Processing key3: {} -> {}", key3, &value3);
                    local_stats.combine(&stats3);
                }
            }
        }
        //let mut stats = all_stats.lock().unwrap();
        //if !stats.contains_key(key0) {
        //    stats.insert(key0, Stats::new());
        //}
        //stats.get_mut(key0).unwrap().combine(&local_stats);
        local_stats
    }).collect_into_vec(&mut local_stats);
    // We need to account for the last character in our stats here...
    let mut total_stats = Stats::new();
    for local in local_stats {
        total_stats.combine(&local);
    }
    // Account for the last character in our stats
    *total_stats.all.get_mut(&template.chars().last().unwrap()).unwrap() += 1;
    //*stats.all.get_mut(&template.chars().last().unwrap()).unwrap() += 1;
    total_stats.update();
    total_stats
}

fn polymer_stats(polymer: &str, count_last: bool) -> Stats {
    let mut cache = polymer.chars().fold(HashMap::new(), |mut table: HashMap<char, usize>, c: char| {
        *table.entry(c).or_default() += 1;
        table
    });
    if !count_last {
        let c = polymer.chars().last().unwrap();
        *cache.get_mut(&c).unwrap() -= 1; 
    }
    let mut stats = Stats::new();
    let most = cache.iter().max_by_key(|entry| entry.1).unwrap();
    let least = cache.iter().min_by_key(|entry| entry.1).unwrap();
    stats.most = *most.0;
    stats.most_len = *most.1;
    stats.least = *least.0;
    stats.least_len = *least.1;
    stats.all = cache;
    stats
}

fn build_cache(rules: &HashMap<String, char>, iterations: usize) -> HashMap<String, (String, Stats)> {
    let mut cache = HashMap::new();
    for key in rules.keys() {
        let value = expand_pair(key, &rules, iterations);
        let stats = polymer_stats(&value, false);
        //println!("\n{} (iters {}) => {}\n", key, iterations, value);
        cache.insert(key.to_string(), (value.to_string(), stats));
    }
    cache
}

fn expand_pair(pair: &str, rules: &HashMap<String, char>, iterations: usize) -> String {
    let mut expanded = pair.to_string();
    for i in 0..iterations {
        let mut s = String::new();
        for i in 0..expanded.len()-1 {
            let key = &expanded[i..=i+1];
            let ins = rules.get(key).unwrap();
            s.push(expanded.chars().nth(i).unwrap());
            s.push(*ins);
        }
        s.push(expanded.chars().last().unwrap());
        expanded = s;
    }
    expanded
}

/*
fn element_stats(polymer: &str) -> Vec<(char, usize)> {
    let mut counts = HashMap::new();
    let _ = polymer.chars().map(|c| *counts.entry(c).or_insert(0) += 1).collect::<Vec<_>>();
    let mut stats = Vec::new();
    for (key, count) in counts.iter() {
        stats.push((key.clone(), count.to_owned()));
    }
    stats.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
    stats
}
*/

fn read_input() -> Result<(String, HashMap<String, char>)> {
    let contents = fs::read_to_string("input")?;
    //let contents = fs::read_to_string("input-small")?;
    //let contents = fs::read_to_string("input-small2")?;
    let template = contents.lines().take(1).collect::<String>();
    let mut rules = HashMap::new();
    for rule in contents.lines().skip(1).filter(|l| !l.is_empty()).map(|l| l.split(" -> ").collect::<Vec<_>>()) {
        rules.insert(rule[0].to_string(), rule[1].chars().nth(0).unwrap());
    }
    Ok((template, rules))
}   
