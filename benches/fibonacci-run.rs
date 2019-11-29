mod common;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const INPUT: &'static str = include_str!("fibonacci.sl");

pub fn criterion_benchmark(c: &mut Criterion) {
    let func = common::generate(common::check(common::parse(INPUT)));

    c.bench_function("fibonacci-run", |b| {
        b.iter(|| common::run(black_box(func.clone())))
    });
}

criterion_group!(
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = criterion_benchmark
);
criterion_main!(benches);
