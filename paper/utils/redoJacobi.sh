#!/usr/bin/env bash

./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi True
./benchmarkCalculator ../raw_benches/bench_jacobi_big.csv bench_jacobi True
cp bench-jacobi.bench.* ../content/benchmarks/distributed-jacobi

./calculateDifferences bench-jacobi.bench.jacobi-eden-3-3217.csv bench-jacobi.bench.jacobi-parr-3-3217.csv 3-3217-diff.csv
./calculateDifferences bench-jacobi.bench.jacobi-eden-3-4253.csv bench-jacobi.bench.jacobi-parr-3-4253.csv 3-4253-diff.csv

cp 3-3217-diff.csv ../content/benchmarks/distributed-jacobi
cp 3-4253-diff.csv ../content/benchmarks/distributed-jacobi

rm *.csv