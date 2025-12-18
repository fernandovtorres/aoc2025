use std::fs;

#[derive(Debug, Clone, Copy)]
struct Range(u64, u64);

fn parse() -> (Vec<Range>, Vec<u64>) {
    let content = fs::read_to_string("../../inputs/day5.txt").expect("vish");

    let mut ranges: Vec<Range> = Vec::new();
    let mut queries: Vec<u64> = Vec::new();


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
        } else {
            if let Ok(num) = tr.parse::<u64>() {
                queries.push(num);
            };
        };
    };

    (ranges, queries)
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
    let (ranges, queries) = parse();

    let mer = merge(ranges);

    let mut co = 0;

    for &que in &queries {
        let i = mer.partition_point(|r| r.0 <= que);

        let valid = if i > 0 {
            let res = &mer[i-1];
            que <= res.1
        } else {
            false
        };

        if valid { co += 1};
    }

    println!("{co}");
}
