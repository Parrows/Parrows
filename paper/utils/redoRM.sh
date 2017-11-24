#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_distributed_rm.csv bench_distributed True
cp bench-distributed.bench.* ../content/benchmarks/distributed-rm

originalBenchmarks=(
    "bench-distributed.bench.skelrm-eden-44497-256.csv"
)

parrowsBenchmarks=(
    "bench-distributed.bench.skelrm-parrows-44497-256.csv"
)

outFileNames=(
    "44497-256-diff.csv"
)

displayNames=(
    "\"Eden vs. PArrows 44497 256\""
)

vs=(
    "\"Eden\""
)

params=(
    "\"44497 256\""
)

benchmark="\"Rabin-Miller (Distributed)\""

bestAndWorstFileName=(
    "bestAndWorstRM-44497-256.csv"
)

outputFolder="../content/benchmarks/distributed-rm"

count=${#originalBenchmarks[@]}

for i in $(seq 0 $(expr ${count} - 1));
do
    cp header.txt ${bestAndWorstFileName[i]}
done

for i in $(seq 0 $(expr ${count} - 1));
do
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${outFileNames[i]}
    cp ${outFileNames[i]} ${outputFolder}

    echo -n "${benchmark},${vs[i]},${params[i]},${displayNames[i]}," >> ${bestAndWorstFileName[i]}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${bestAndWorstFileName[i]} True
done


for i in $(seq 0 $(expr ${count} - 1));
do
    cp ${bestAndWorstFileName[i]} ${outputFolder}
done

rm *.csv