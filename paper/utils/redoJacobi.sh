#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_jacobi.csv bench_jacobi True
./benchmarkCalculator ../raw_benches/bench_jacobi_big.csv bench_jacobi True
cp bench-jacobi.bench.* ../content/benchmarks/distributed-jacobi

originalBenchmarks=(
    "bench-jacobi.bench.jacobi-eden-3-3217.csv"
    "bench-jacobi.bench.jacobi-eden-3-4253.csv"
)

parrowsBenchmarks=(
    "bench-jacobi.bench.jacobi-parr-3-3217.csv"
    "bench-jacobi.bench.jacobi-parr-3-4253.csv"
)

outFileNames=(
    "3-3217-diff.csv"
    "3-4253-diff.csv"
)

displayNames=(
    "\"Eden vs. PArrows 3217\""
    "\"Eden vs. PArrows 4253\""
)

benchmark="\"Jacobi (Distributed)\""

worstFileName="worstJacobi.csv"
bestFileName="bestJacobi.csv"

outputFolder="../content/benchmarks/distributed-jacobi"

count=${#originalBenchmarks[@]}

cp header.txt ${worstFileName}
cp header.txt ${bestFileName}

for i in $(seq 0 $(expr ${count} - 1));
do
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${outFileNames[i]}
    cp ${outFileNames[i]} ${outputFolder}

    echo -n "${benchmark},${displayNames[i]}," >> ${worstFileName}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${worstFileName} True True

    echo -n "${benchmark},${displayNames[i]}," >> ${bestFileName}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${bestFileName} True False
done

cp ${worstFileName} ${outputFolder}
cp ${bestFileName} ${outputFolder}

rm *.csv