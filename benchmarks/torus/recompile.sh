#!/bin/bash

rm *.o
rm *.hi

ghc matrix_seq.hs

stack ghc torus_matrix_parrows_multicore.hs -- -threaded

stack ghc torus_matrix_parrows_parmonad.hs -- -threaded

ghc torus_matrix_parrows_eden -parcp -o torus_matrix_parrows_eden_sm
ghc torus_matrix_eden -parcp -o torus_matrix_eden_sm
