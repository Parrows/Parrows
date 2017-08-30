#!/bin/bash

echo "cleaning up stack environment"
stack clean
stack install

echo "cleaning up previous compiled versions"
# force recompile by cleaning up manually
rm *.o
rm *.hi

echo "Compiling..."
ghc torus_matrix_parrows -parmpi -rtsopts
ghc torus_matrix_eden -parmpi -rtsopts
#stack ghc torus_matrix_multicore -- -threaded -rtsopts
echo "done."

procCounts=(
    "256"
    "128"
    "64"
    "32"
    "1"
#    "48"
#    "64"
#    "80"
#    "96"
)

programs=(
    "torus_matrix_parrows"
    "torus_matrix_eden"
    #"torus_matrix_multicore" 
)

problemSizes=(
    #"32"
    #"64"
   #"128"
    #"256"
    #"512"
#    "2048"
    "4096"
    "8192"
)

torusSizes=(
    "256"
    #"2"
    #"4"
    #"8"
    #"16"
    #"32"
    #"64"
)

# get length of an array
programCount=${#programs[@]}

for problemSize in "${problemSizes[@]}"
do
    for torusSize in "${torusSizes[@]}"
    do
        for procCount in "${procCounts[@]}"
        do
            # benchCmds=""
            for program in "${programs[@]}"
            do
                cmd="\"./"${program}" "${torusSize}" "${problemSize}" +RTS -N"${procCount}"\""
                benchCmds=${benchCmds}" "${cmd}
            done
        done
    done
done

#evalCmd="bench "${benchCmds}" -o probSize=${problemSize}_torusSize=${torusSize}_procCount=${procCount}.html --resamples 1000"
evalCmd="bench "${benchCmds}" --csv benchResults.csv" 
echo "running ${evalCmd}"
eval "$evalCmd"
