#!/bin/bash

rtsOpts="+RTS -N16"

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
    "sudoku17.16000.txt"
    "sudoku17.49151.txt"
)

# get length of an array
programCount=${#programs[@]}

for puzzle in "${puzzles[@]}"
do
    echo ${puzzle}
    for (( i=0; i < ${programCount}; i++ ));
    do
        progName=${programs[$i]}
        cmd="./"${progName}" "${puzzle}" "${parameters[$i]}" "${rtsOpts}

        eval ${cmd}
    done

    echo ""
    echo ""
    echo ""
done
