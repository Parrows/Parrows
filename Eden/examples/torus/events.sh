#!/bin/bash
ghc torus_matrix_parrows -parmpi -rtsopts -eventlog
ghc torus_matrix_eden -parmpi -rtsopts -eventlog

./torus_matrix_eden +RTS -N16 -ls
./torus_matrix_parrows +RTS -N16 -ls
