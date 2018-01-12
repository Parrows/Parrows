#!/bin/bash

procCount=16

programs=(
     "torus_matrix_eden_sm"
     "torus_matrix_parrows_eden_sm"
     "torus_matrix_parrows_multicore"
     "torus_matrix_parrows_parmonad"
     "matrix_seq"
)

parameters=(
    "512"
)

./recompile.sh

# get length of an array
programCount=${#programs[@]}

benchCmds=""

for parameter in "${parameters[@]}"
do
    for (( i=0; i < ${programCount}; i++ ));
    do
        progName=${programs[$i]}
        if [ "${progName}" == "matrix_seq" ]
        then
            cmd="\"./"${progName}" "${parameter}"\""
            benchCmds=${benchCmds}" "${cmd}
        else
            for (( j=${procCount}; j>=1; j=j/2 ));
            do
                cmd="\"./"${progName}" "${parameter}" "${j}" +RTS -N"${j}"\""

                benchCmds=${benchCmds}" "${cmd}
            done
        fi
    done
done

echo "running: bench"${benchCmds}" --csv bench.csv"

eval "bench"${benchCmds}" --csv bench.csv"
