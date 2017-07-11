#!/bin/bash

rm *.o
rm *.hi

stack ghc sudoku-seq.hs

stack ghc parmonad-sudoku.hs -- -threaded

ghc parrows-sudoku-parmap-eden.hs -parcp

stack ghc parrows-sudoku-parmap-mult.hs -- -threaded
stack ghc parrows-sudoku-parmap-par.hs -- -threaded
