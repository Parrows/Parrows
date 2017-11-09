#!/bin/bash

procCount=32

programs=(
#    "skelrm-parr-eden-cp"
#    "skelrm-parr-mult"
#1    "skelrm-parr-par"
)

parameters=(
#    "4423 32"
#    "4423 64"
#    "9941 32" 
#    "9941 64"
    "11213 32" 
    "11213 64"
)

# get length of an array
programCount=${#programs[@]}

benchCmds=""

./compile.sh

for parameter in "${parameters[@]}"
do
    for (( i=0; i < ${programCount}; i++ ));
    do
        progName=${programs[$i]}
        if [ "${progName}" == "skelrm-seq" ]
        then
            cmd="\"./"${progName}" "${parameter}"\""
            benchCmds=${benchCmds}" "${cmd}
        else
            for (( j=${procCount}; j>=1; j=j/2 ));
            do
                cmd="\"./"${progName}" "${parameter}" +RTS -N"${j}"\""

                benchCmds=${benchCmds}" "${cmd}
            done
        fi
    done
done

echo "running: bench"${benchCmds}" --csv bench.sm.csv"

eval "bench"${benchCmds}" --csv bench.sm.csv"
