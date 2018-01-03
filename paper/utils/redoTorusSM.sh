#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_torus_sm.csv bench_torus_sm True
cp bench-torus-sm.bench.* ../content/benchmarks/torus-sm

originalBenchmarks=(
    "bench-torus-sm.bench.torus-matrix-eden-sm-512.csv"
)

parrowsBenchmarks=(
    "bench-torus-sm.bench.torus-matrix-parrows-eden-sm-512.csv"
)

outFileNames=(
    "eden-cp-512-diff.csv"
)

displayNames=(
    "\"Eden vs. PArrows 512\""
)

vs=(
    "\"Eden CP\""
)

params=(
    "\"512\""
)

benchmark="\"Torus (Shared-Memory)\""

bestAndWorstFileName=(
    "bestAndWorstTorusSM-512.csv"
)

outputFolder="../content/benchmarks/torus-sm"

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
