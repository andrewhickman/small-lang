mod common;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const INPUT: &'static str = include_str!("../data/fibonacci.sl");

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci-parse", |b| {
        b.iter(|| common::parse(black_box(INPUT)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
