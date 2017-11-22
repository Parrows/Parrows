#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_torus.csv bench_torus_distributed True
cp bench-torus-distributed.bench.* ../content/benchmarks/distributed-torus

originalBenchmarks=(
    "bench-torus-distributed.bench.torus-matrix-eden-4096.csv"
)

parrowsBenchmarks=(
    "bench-torus-distributed.bench.torus-matrix-parrows-4096.csv"
)

outFileNames=(
    "4096-diff.csv"
)

worstFileName="worstTorus.csv"
bestFileName="bestTorus.csv"

outputFolder="../content/benchmarks/distributed-torus"

count=${#originalBenchmarks[@]}

touch ${worstFileName}
touch ${bestFileName}

for i in $(seq 0 $(expr ${count} - 1));
do
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${outFileNames[i]}
    cp ${outFileNames[i]} ${outputFolder}

    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${worstFileName} True True
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${bestFileName} True False
done

cp ${worstFileName} ${outputFolder}
cp ${bestFileName} ${outputFolder}

rm *.csv
