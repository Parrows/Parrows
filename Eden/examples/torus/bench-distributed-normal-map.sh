#!/bin/bash

#echo "cleaning up stack environment"
#stack clean
#stack install

echo "cleaning up previous compiled versions"
# force recompile by cleaning up manually
rm *.o
rm *.hi

echo "Compiling..."
ghc torus_matrix_parrows_normalmap -parmpi -rtsopts
ghc torus_matrix_eden_normalmap -parmpi -rtsopts
#stack ghc torus_matrix_multicore -- -threaded -rtsopts
echo "done."

procCounts=(
    "512"
    "256"
    "128"
    "64"
    "32"
#    "16"
#    "1"
)

programs=(
    "torus_matrix_parrows_normalmap"
    "torus_matrix_eden_normalmap"
#    "torus_matrix_multicore" 
)

#hack
problemSizes=(
    "32 512 64 200"
    "32 1024 64 200"
    "64 1024 128 200"
)

# get length of an array
programCount=${#programs[@]}

for problemSize in "${problemSizes[@]}"
do
    for procCount in "${procCounts[@]}"
    do
        # benchCmds=""
        for program in "${programs[@]}"
        do
            cmd="\"./"${program}" "${problemSize}" +RTS -N"${procCount}"\""
            benchCmds=${benchCmds}" "${cmd}
        done
    done
done

#evalCmd="bench "${benchCmds}" -o probSize=${problemSize}_torusSize=${torusSize}_procCount=${procCount}.html --resamples 1000"
evalCmd="bench "${benchCmds}" --csv bench.distributed.normalmap.csv"
echo "running ${evalCmd}"
eval "$evalCmd"
