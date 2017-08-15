#!/usr/bin/env bash

./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus_distributed True
cp bench-torus-distributed.bench.* ../content/benchmarks/distributed-torus

./benchmarkCalculator ../raw_benches/sudoku_sm.csv bench_sudoku_sm True
cp bench-sudoku-sm.bench.* ../content/benchmarks/sudoku-sm

./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi True
cp bench-jacobi.bench.* ../content/benchmarks/distributed-jacobi

./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus True
cp bench-torus.bench.* ../content/benchmarks/torus

#./calculateDifferences bench-torus-distributed.bench.torus-matrix-eden-32-512-64-200.csv bench-torus-distributed.bench.torus-matrix-parrows-32-512-64-200.csv 32-512-64-200-diff.csv
./calculateDifferences bench-torus-distributed.bench.torus-matrix-eden-1024.csv bench-torus-distributed.bench.torus-matrix-parrows-1024.csv 1024-diff.csv

./calculateDifferences bench-jacobi.bench.jacobi-eden-3-1279.csv bench-jacobi.bench.jacobi-parr-3-1279.csv 3-1279-diff.csv
#./calculateDifferences bench-distributed.bench.skelrm-eden-11213-256.csv bench-distributed.bench.skelrm-parrows-11213-256.csv 11213-256-diff.csv
#./calculateDifferences bench-distributed.bench.skelrm-eden-44497-256.csv bench-distributed.bench.skelrm-parrows-44497-256.csv 44497-256-diff.csv

cp 1024-diff.csv ../content/benchmarks/distributed-torus
#cp 32-1024-64-200-diff.csv ../content/benchmarks/distributed-torus

cp 3-1279-diff.csv ../content/benchmarks/distributed-jacobi
# cp 44497-256-diff.csv ../content/benchmarks/distributed-rm

rm *.csv
