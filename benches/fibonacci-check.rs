mod common;

use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};

const INPUT: &'static str = include_str!("../data/fibonacci.sl");

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci-check", |b| {
        b.iter_batched(
            || common::parse(black_box(INPUT)),
            common::check,
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
