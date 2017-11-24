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

worstFileName=(
    "worstRM-44497-256.csv"
)
bestFileName=(
    "bestRM-44497-256.csv"
)

outputFolder="../content/benchmarks/distributed-rm"

count=${#originalBenchmarks[@]}

for i in $(seq 0 $(expr ${count} - 1));
do
    cp header.txt ${worstFileName[i]}
    cp header.txt ${bestFileName[i]}
done

for i in $(seq 0 $(expr ${count} - 1));
do
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${outFileNames[i]}
    cp ${outFileNames[i]} ${outputFolder}

    echo -n "${benchmark},${vs[i]},${params[i]},${displayNames[i]}," >> ${worstFileName}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${worstFileName} True True

    echo -n "${benchmark},${vs[i]},${params[i]},${displayNames[i]}," >> ${bestFileName}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${bestFileName} True False
done


for i in $(seq 0 $(expr ${count} - 1));
do
    cp ${worstFileName[i]} ${outputFolder}
    cp ${bestFileName[i]} ${outputFolder}
done

rm *.csv