#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi True
./benchmarkCalculator ../raw_benches/bench_jacobi_big.csv bench_jacobi True
cp bench-jacobi.bench.* ../content/benchmarks/distributed-jacobi

originalBenchmarks=(
    "bench-jacobi.bench.jacobi-eden-3-3217.csv"
)

parrowsBenchmarks=(
    "bench-jacobi.bench.jacobi-parr-3-3217.csv"
)

outFileNames=(
    "3-3217-diff.csv"
)

displayNames=(
    "\"Eden vs. PArrows 3217\""
)

vs=(
    "\"Eden\""
)

params=(
    "\"3217\""
)

benchmark="\"Jacobi (Distributed)\""

bestAndWorstFileName=(
    "bestAndWorstJacobi-3217.csv"
)

outputFolder="../content/benchmarks/distributed-jacobi"

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
