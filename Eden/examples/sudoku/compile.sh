#!/bin/bash

programs=(
    "parrows-sudoku-parmap"
    "parrows-sudoku-parmapstream"
    "parrows-sudoku-farm"
    "parrows-sudoku-farmChunk"
)

# get length of an array
programCount=${#programs[@]}

for (( i=0; i < ${programCount}; i++ ));
do
    progName=${programs[$i]}
    cmd="ghc "$progName" -parmpi -rtsopts -eventlog -O2"
    tmp=$(eval $cmd)
    echo ${tmp}
done

