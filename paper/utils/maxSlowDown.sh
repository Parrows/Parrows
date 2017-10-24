#!/usr/bin/env bash

./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus_distributed True
./benchmarkCalculator ../raw_benches/sudoku_sm.csv bench_sudoku_sm True
./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi True
./benchmarkCalculator ../raw_benches/bench_distributed_rm.csv bench_distributed True
./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus True
./benchmarkCalculator ../raw_benches/bench_jacobi_big.csv bench_jacobi_distributed True
./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi_distributed True

./calculateDifferences bench-torus-distributed.bench.torus-matrix-eden-4096.csv bench-torus-distributed.bench.torus-matrix-parrows-4096.csv 4096-diff.csv.out
./calculateDifferences bench-jacobi.bench.jacobi-eden-3-3217.csv bench-jacobi.bench.jacobi-parr-3-3217.csv 3-3217-diff.csv.out
./calculateDifferences bench-distributed.bench.skelrm-eden-44497-256.csv bench-distributed.bench.skelrm-parrows-44497-256.csv 44497-256-diff.csv.out
./calculateDifferences bench-jacobi-distributed.bench.jacobi-eden-3-4253.csv bench-jacobi-distributed.bench.jacobi-parr-3-4253.csv jacobi-4253-diff.csv.out
./calculateDifferences bench-jacobi-distributed.bench.jacobi-eden-3-3217.csv bench-jacobi-distributed.bench.jacobi-parr-3-3217.csv jacobi-3217-diff.csv.out

rm *.csv