#!/bin/bash

rtsOpts="+RTS -N4 -s"

## declare an array variable
programs=(
    "parrows-sudoku-parmap"
    "parrows-sudoku-parmapstream"
    "parrows-sudoku-farm"
    "parrows-sudoku-farmChunk"
)

parameters=(
    ""
    "16"
    "4"
    "16 4"
)

puzzles=(
    "sudoku17.1000.txt"
    "sudoku17.16000.txt"
    "sudoku17.49151.txt"
)

runsPerConfig=10

# get length of an array
programCount=${#programs[@]}

for puzzle in "${puzzles[@]}"
do
    echo $puzzle
    for (( i=0; i < ${programCount}; i++ ));
    do
        progName=${programs[$i]}
        cmd="./"$progName" "$puzzle" "${parameters[$i]}" "$rtsOpts
        #echo "running: " $cmd
        grepCmd="2>&1 >/dev/null | grep Total | sed \"s/Total[a-z ]*[0-9.s]*[ ]*[( ]*\([0-9.]*\)[ a-z)]*/\\1/\" | awk '{\$1=\$1};1\'"

        totalTime=0

        for (( j = 0; j < $runsPerConfig; j++ ));
        do
            curTime=$(eval $cmd" "$grepCmd)
            #echo $curTime

            totalTime=`echo $totalTime + $curTime | bc`
        done

        average=`echo "scale=3;$totalTime / $runsPerConfig" | bc`

        echo $progName" "$average
    done

    echo ""
    echo ""
    echo ""
done

