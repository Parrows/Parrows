#!/bin/bash

cd jacobi-parrows

rm *.o
rm *.hi

ghc --make -O2 -parmpi NewSkelJacobiSum.lhs -o ../jacobi-parr

cd ../

cd jacobi

rm *.o
rm *.hi

ghc --make -O2 -parmpi NewSkelJacobiSum.lhs -o ../jacobi-eden

cd ../

