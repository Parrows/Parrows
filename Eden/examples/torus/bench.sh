#!/bin/bash
ghc torus_matrix_parrows -parmpi -eventlog -rtsopts
ghc torus_matrix_eden -parmpi -eventlog -rtsopts

bench "./torus_matrix_parrows +RTS -N96" "./torus_matrix_parrows +RTS -N64" "./torus_matrix_parrows +RTS -N48" "./torus_matrix_parrows +RTS -N32" "./torus_matrix_parrows +RTS -N16" "./torus_matrix_parrows +RTS -N8" "./torus_matrix_parrows +RTS -N4" "./torus_matrix_parrows +RTS -N2" -o bench_parrows.html --resamples 10000
bench "./torus_matrix_eden +RTS -N96" "./torus_matrix_eden +RTS -N64" "./torus_matrix_eden +RTS -N48" "./torus_matrix_eden +RTS -N32" "./torus_matrix_eden +RTS -N16" "./torus_matrix_eden +RTS -N8" "./torus_matrix_eden +RTS -N4" "./torus_matrix_eden +RTS -N2" -o bench_eden.html --resamples 10000

