#/bin/bash

./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus_distributed True
cp bench-torus-distributed.bench.* ../content/benchmarks/distributed-torus

./benchmarkCalculator ../raw_benches/sudoku_sm.csv bench_sudoku_sm True
cp bench-sudoku-sm.bench.* ../content/benchmarks/sudoku-sm

./calculateDifferences bench-torus-distributed.bench.torus-matrix-eden-32-512-64-200.csv bench-torus-distributed.bench.torus-matrix-parrows-32-512-64-200.csv 32-512-64-200-diff.csv
./calculateDifferences bench-torus-distributed.bench.torus-matrix-eden-32-1024-64-200.csv bench-torus-distributed.bench.torus-matrix-parrows-32-1024-64-200.csv 32-1024-64-200-diff.csv
#./calculateDifferences bench-distributed.bench.skelrm-eden-11213-256.csv bench-distributed.bench.skelrm-parrows-11213-256.csv 11213-256-diff.csv
#./calculateDifferences bench-distributed.bench.skelrm-eden-44497-256.csv bench-distributed.bench.skelrm-parrows-44497-256.csv 44497-256-diff.csv

cp 32-512-64-200-diff.csv ../content/benchmarks/distributed-torus
cp 32-1024-64-200-diff.csv ../content/benchmarks/distributed-torus
# cp 44497-256-diff.csv ../content/benchmarks/distributed-rm

rm *.csv
