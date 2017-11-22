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

benchmark="\"Rabin-Miller (Distributed)\""

worstFileName="worstRM.csv"
bestFileName="bestRM.csv"

outputFolder="../content/benchmarks/distributed-rm"

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
