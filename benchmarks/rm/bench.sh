#!/bin/bash

procCount=512

programs=(
    "skelrm-parrows"
    "skelrm-eden"
    "skelrm-seq"
)

parameters=(
    "44497 256"
)

# get length of an array
programCount=${#programs[@]}

benchCmds=""

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
            for (( j=${procCount}; j>=16; j=j/2 ));
            do
                cmd="\"./"${progName}" "${parameter}" +RTS -N"${j}"\""

                benchCmds=${benchCmds}" "${cmd}
            done
	    cmd="\"./"${progName}" "${parameter}" +RTS -N192\""
	    benchCmds=${benchCmds}" "${cmd}
        fi
    done
done

echo "running: bench"${benchCmds}" --csv bench.csv"

eval "bench"${benchCmds}" --csv bench.csv"
