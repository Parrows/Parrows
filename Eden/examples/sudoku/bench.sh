#!/bin/bash

procCount=16

programs=(
    "parrows-sudoku-parmap"
    "parrows-sudoku-parmapstream"
    "parrows-sudoku-farm"
    "parrows-sudoku-farmChunk"
)

parameters=(
    ""
    "384"
    "12"
    "384 12"
)

puzzles=(
    "sudoku17.1000.txt"
    #"sudoku17.16000.txt"
    #"sudoku17.49151.txt"
)

# get length of an array
programCount=${#programs[@]}

benchCmds=""

for puzzle in "${puzzles[@]}"
do
    for (( i=0; i < ${programCount}; i++ ));
    do
        for (( j=1; j <= ${procCount}; j++ ));
        do
            progName=${programs[$i]}
            cmd="\"./"${progName}" "${puzzle}" "${parameters[$i]}" +RTS -N"${j}"\""

            benchCmds=${benchCmds}" "${cmd}
        done
    done
done

eval "bench"${benchCmds}" -o bench.html"
