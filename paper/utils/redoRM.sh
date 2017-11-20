#!/usr/bin/env bash

./benchmarkCalculator ../raw_benches/bench_distributed_rm.csv bench_distributed True
cp bench-distributed.bench.* ../content/benchmarks/distributed-rm

./calculateDifferences bench-distributed.bench.skelrm-eden-44497-256.csv bench-distributed.bench.skelrm-parrows-44497-256.csv 44497-256-diff.csv

cp 44497-256-diff.csv ../content/benchmarks/distributed-rm

rm *.csv
