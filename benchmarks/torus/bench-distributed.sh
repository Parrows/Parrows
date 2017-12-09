#!/bin/bash

#echo "cleaning up stack environment"
#stack clean
#stack install

echo "cleaning up previous compiled versions"
# force recompile by cleaning up manually
rm *.o
rm *.hi

echo "Compiling..."
ghc torus_matrix_parrows_eden.hs -parmpi -rtsopts
ghc torus_matrix_eden.hs -parmpi -rtsopts
#stack ghc torus_matrix_multicore -- -threaded -rtsopts
echo "done."

procCounts=(
#    "512"
    "256"
    "224"
    "192"
    "160" 
    "128"
    "96"
    "64"
    "32"
    "16"
#    "1"
)

programs=(
    "torus_matrix_parrows_eden"
    "torus_matrix_eden"
)

#hack
problemSizes=( 
#    "512"
#    "1024"
#     "2048"
     "4096"
     "8192"
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
            cmd="\"./"${program}" "${problemSize}" +RTS -N"${procCount}" -qQ104857600\""
            benchCmds=${benchCmds}" "${cmd}
        done
    done
done

#evalCmd="bench "${benchCmds}" -o probSize=${problemSize}_torusSize=${torusSize}_procCount=${procCount}.html --resamples 1000"
evalCmd="bench "${benchCmds}" --csv bench.csv"
echo "running ${evalCmd}"
eval "$evalCmd"
