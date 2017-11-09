#!/bin/bash

rm *.o
rm *.hi

stack ghc sudoku-seq.hs

stack ghc parmonad-sudoku.hs -- -threaded
stack ghc parrows-sudoku-parmap-par.hs -- -threaded

ghc parrows-sudoku-parmap-eden.hs -parcp
ghc eden-sudoku.hs -parcp

stack ghc multicore-sudoku.hs -- -threaded
stack ghc parrows-sudoku-parmap-mult.hs -- -threaded
