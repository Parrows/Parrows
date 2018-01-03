#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus_distributed True
cp bench-torus-distributed.bench.* ../content/benchmarks/distributed-torus

originalBenchmarks=(
    "bench-torus-distributed.bench.torus-matrix-eden-4096.csv"
)

parrowsBenchmarks=(
    "bench-torus-distributed.bench.torus-matrix-parrows-eden-4096.csv"
)

outFileNames=(
    "4096-diff.csv"
)

displayNames=(
    "\"Eden vs. PArrows 4096\""
)

vs=(
    "\"Eden\""
)

params=(
    "\"4096\""
)

benchmark="\"Torus (Distributed)\""

bestAndWorstFileName=(
    "bestAndWorstTorus-4096.csv"
)

outputFolder="../content/benchmarks/distributed-torus"

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
