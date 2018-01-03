#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_torus_sm.csv bench_torus_sm True
cp bench-torus-sm.bench.* ../content/benchmarks/torus-sm
