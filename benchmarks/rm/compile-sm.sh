#!/bin/bash

rm *.o
rm *.hi

stack ghc SkelRMSeq.lhs -- --make -O2 -rtsopts -eventlog -XHaskell98 -o skelrm-seq

ghc --make -O2 -rtsopts -eventlog -parcp -XHaskell98 SkelRMEden.lhs -o skelrm-eden-cp
ghc --make -O2 -rtsopts -eventlog -parcp -XHaskell98 SkelRMParr.lhs -o skelrm-parrows-cp

stack ghc SkelRMParrPar.lhs -- --make -O2 -rtsopts -eventlog -XHaskell98 -o skelrm-parr-par -threaded
stack ghc SkelRMParMonad.lhs -- --make -O2 -rtsopts -eventlog -XHaskell98 -o skelrm-par -threaded

stack ghc SkelRMParrMult.lhs -- --make -O2 -rtsopts -eventlog -XHaskell98 -o skelrm-parr-mult -threaded
stack ghc SkelRMMulticore.lhs -- --make -O2 -rtsopts -eventlog -XHaskell98 -o skelrm-mult -threaded
