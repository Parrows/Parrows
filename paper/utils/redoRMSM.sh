#!/usr/bin/env bash

./preCalculate.sh

./benchmarkCalculator ../raw_benches/bench_sm_rm.csv bench_sm_rm True
cp bench-sm-rm.bench.* ../content/benchmarks/sm-rm

originalBenchmarks=(
    "bench-sm-rm.bench.skelrm-eden-cp-11213-32.csv"
    "bench-sm-rm.bench.skelrm-eden-cp-11213-64.csv"
    "bench-sm-rm.bench.skelrm-mult-11213-32.csv"
    "bench-sm-rm.bench.skelrm-mult-11213-64.csv"
    "bench-sm-rm.bench.skelrm-par-11213-32.csv"
    "bench-sm-rm.bench.skelrm-par-11213-64.csv"
)

parrowsBenchmarks=(
    "bench-sm-rm.bench.skelrm-parr-eden-cp-11213-32.csv"
    "bench-sm-rm.bench.skelrm-parr-eden-cp-11213-64.csv"
    "bench-sm-rm.bench.skelrm-parr-mult-11213-32.csv"
    "bench-sm-rm.bench.skelrm-parr-mult-11213-64.csv"
    "bench-sm-rm.bench.skelrm-parr-par-11213-32.csv"
    "bench-sm-rm.bench.skelrm-parr-par-11213-64.csv"
)

outFileNames=(
    "eden-cp-11213-32-diff.csv"
    "eden-cp-11213-64-diff.csv"
    "mult-11213-32-diff.csv"
    "mult-11213-64-diff.csv"
    "par-11213-32-diff.csv"
    "par-11213-64-diff.csv"
)

displayNames=(
    "\"Eden CP vs. PArrows 11213 32\""
    "\"Eden CP vs. PArrows 11213 64\""
    "\"GpH vs. PArrows 11213 32\""
    "\"GpH vs. PArrows 11213 64\""
    "\"Par Monad vs. PArrows 11213 32\""
    "\"Par Monad vs. PArrows 11213 64\""
)

vs=(
    "\"Eden CP\""
    "\"Eden CP\""
    "\"GpH\""
    "\"GpH\""
    "\"Par Monad \""
    "\"Par Monad \""
)

params=(
    "\"11213 32\""
    "\"11213 64\""
    "\"11213 32\""
    "\"11213 64\""
    "\"11213 32\""
    "\"11213 64\""
)

benchmark="\"Rabin-Miller (Shared-Memory)\""

worstFileName=(
    "worstRMSM-11213-32.csv"
    "worstRMSM-11213-64.csv"
    "worstRMSM-11213-32.csv"
    "worstRMSM-11213-64.csv"
    "worstRMSM-11213-32.csv"
    "worstRMSM-11213-64.csv"
)

bestFileName=(
    "bestRMSM-11213-32.csv"
    "bestRMSM-11213-64.csv"
    "bestRMSM-11213-32.csv"
    "bestRMSM-11213-64.csv"
    "bestRMSM-11213-32.csv"
    "bestRMSM-11213-64.csv"
)

outputFolder="../content/benchmarks/sm-rm"

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

    echo -n "${benchmark},${vs[i]},${params[i]},${displayNames[i]}," >> ${worstFileName[i]}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${worstFileName[i]} True True

    echo -n "${benchmark},${vs[i]},${params[i]},${displayNames[i]}," >> ${bestFileName[i]}
    ./calculateDifferences ${originalBenchmarks[i]} ${parrowsBenchmarks[i]} ${bestFileName[i]} True False
done


for i in $(seq 0 $(expr ${count} - 1));
do
    cp ${worstFileName[i]} ${outputFolder}
    cp ${bestFileName[i]} ${outputFolder}
done

rm *.csv