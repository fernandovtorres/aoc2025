use std::fs;

#[derive(Debug, Clone, Copy)]
struct Range(u64, u64);

fn parse() -> Vec<Range> {
    let content = fs::read_to_string("../../inputs/day5.txt").expect("vish");

    let mut ranges: Vec<Range> = Vec::new();


    for line in content.lines() {
        let tr = line.trim();

        if tr.is_empty() {
            continue;
        };
        
        if tr.contains('-') {
            let part: Vec<&str> = tr.split('-').collect();
            if part.len() == 2 {
                let start = part[0].parse::<u64>().unwrap();
                let end = part[1].parse::<u64>().unwrap();
                ranges.push(Range(start, end));
            };
        };
    };

    ranges
}

fn merge(mut ranges: Vec<Range>) -> Vec<Range> {
    ranges.sort_by_key(|r| r.0);

    let mut mer = Vec::new();
    let mut curr = ranges[0];

    for prox in ranges.into_iter().skip(1) {
        if prox.0 <= curr.1 + 1 {
            curr.1 = curr.1.max(prox.1);
        } else {
            mer.push(curr);
            curr = prox;
        }
    }

    mer.push(curr);
    mer
}

fn main() {
    let ranges = parse();

    let mer = merge(ranges);

    let co = mer.into_iter().map(|x| (x.1 - x.0) + 1).reduce(|x, y| x + y).unwrap();
    println!("{co}");
}
