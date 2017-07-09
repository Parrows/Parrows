#!/bin/bash

stack clean
stack install

rm *.o
rm *.hi

stack ghc torus_matrix_multicore.hs -- -threaded -rtsopts

