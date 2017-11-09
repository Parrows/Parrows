#!/bin/bash

rm *.o
rm *.hi

ghc --make -O2 -rtsopts -eventlog -parmpi -XHaskell98 SkelRMParr.lhs -o skelrm-parrows
ghc --make -O2 -rtsopts -eventlog -parmpi -XHaskell98 SkelRMEden.lhs -o skelrm-eden
