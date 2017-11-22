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

worstFileName="worstRMSM.csv"
bestFileName="bestRMSM.csv"

outputFolder="../content/benchmarks/sm-rm"

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