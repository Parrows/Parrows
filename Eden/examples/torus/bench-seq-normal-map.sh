#!/bin/bash

#echo "cleaning up stack environment"
#stack clean
#stack install

echo "cleaning up previous compiled versions"
# force recompile by cleaning up manually
rm *.o
rm *.hi

echo "Compiling..."
#ghc torus_matrix_parrows -parmpi -rtsopts
#ghc torus_matrix_eden -parmpi -rtsopts
stack ghc matrix_seq
#stack ghc torus_matrix_multicore -- -threaded -rtsopts
echo "done."

programs=(
     "matrix_seq"
)

#hack
problemSizes=(
    "64 200"
    "64 200"
    "128 200"
)

# get length of an array
programCount=${#programs[@]}

for problemSize in "${problemSizes[@]}"
do
            # benchCmds=""
        for program in "${programs[@]}"
        do
            cmd="\"./"${program}" "${problemSize}"\""
            benchCmds=${benchCmds}" "${cmd}
        done
done

#evalCmd="bench "${benchCmds}" -o probSize=${problemSize}_torusSize=${torusSize}_procCount=${procCount}.html --resamples 1000"
evalCmd="bench "${benchCmds}" --csv bench.csv"
echo "running ${evalCmd}"
eval "$evalCmd"
