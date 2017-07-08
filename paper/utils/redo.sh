#/bin/bash
./benchmarkCalculator ../raw_benches/bench_distributed_rm.csv bench_distributed True
cp bench-distributed.bench.* ../content/benchmarks/distributed-rm

./benchmarkCalculator ../raw_benches/bench_sm_rm.csv bench_sm_rm True
cp bench-sm-rm.bench.* ../content/benchmarks/sm-rm


./calculateDifferences bench-distributed.bench.skelrm-eden-9941-256.csv bench-distributed.bench.skelrm-parrows-9941-256.csv 9941-256-diff.csv
./calculateDifferences bench-distributed.bench.skelrm-eden-11213-256.csv bench-distributed.bench.skelrm-parrows-11213-256.csv 11213-256-diff.csv
./calculateDifferences bench-distributed.bench.skelrm-eden-44497-256.csv bench-distributed.bench.skelrm-parrows-44497-256.csv 44497-256-diff.csv

cp 9941-256-diff.csv ../content/benchmarks/distributed-rm
cp 11213-256-diff.csv ../content/benchmarks/distributed-rm
cp 44497-256-diff.csv ../content/benchmarks/distributed-rm

rm *.csv