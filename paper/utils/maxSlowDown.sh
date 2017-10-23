#!/usr/bin/env bash

./benchmarkCalculator ../raw_benches/bench_jacobi_big.csv bench_jacobi_distributed True
./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi_distributed True

./calculateDifferences bench-jacobi-distributed.bench.jacobi-eden-3-4253.csv bench-jacobi-distributed.bench.jacobi-parr-3-4253.csv jacobi-4253-diff.csv
./calculateDifferences bench-jacobi-distributed.bench.jacobi-eden-3-3217.csv bench-jacobi-distributed.bench.jacobi-parr-3-3217.csv jacobi-3217-diff.csv