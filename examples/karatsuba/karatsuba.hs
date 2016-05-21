{-# OPTIONS -cpp #-}
-- |
-- Module      :  Karatsuba
-- Copyright   :  (c) Philipps Universitaet Marburg 2009-2010
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  eden@mathematik.uni-marburg.de
-- Stability   :  beta
-- Portability :  not portable
--
-- The following Haskell module implements a parallel version of the Karatsuba algorithm with Eden.
--
-- Depends on the Eden Compiler.
--
-- Eden Project

module Main where

import NewDC

import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment (getArgs)

usage = "I need 4 parameters! version(0-naiveDC,1-DC_Depth,2-DC_Placement,3-dcTickets,4-divConFlat(farm),5-divConFlat(ssf)), parallel depth, " 
	++ "no. of digits (for number 1 and 2)"

main = do args <- getArgs
	  if length args < 4 
	   then putStrLn usage
	   else 
            let i1'  = read i1
                i2' = read i2
                d'  = read d
                t'  = read t
                is  = concat (repeat [1..9])
                mi1 = take i1' is
                mi2 = take i2' is
                result = karat t' d' mi1 mi2
	        (t:d:i1:i2:_) = args
             in rnf result 
		   `seq` 
#ifdef CHECK 
		         print ((fromMyInteger (fromD result)) 
				      == (fromMyInteger mi1)*(fromMyInteger mi2))
#else 
                         print "done" 
#endif

fromMyInteger :: MyInteger -> Integer
fromMyInteger [] = 0
fromMyInteger (i:is) = (fromIntegral i) + (fromIntegral base)*(fromMyInteger is)


data D a = D {fromD :: a} deriving Show
instance NFData a => NFData (D a) where
 rnf (D a) = rnf a
-- instance Trans a => Trans (D a)



type MyInteger = [Int]
base::Int
base = 10   -- Global base for all MyIntegers
type MyIntegerS = (MyInteger,Bool) -- False <==> negative


karat :: Int -> Int -> MyInteger -> MyInteger -> D MyInteger
-- simple DC skeleton, no explicit placement, no deth control
karat 0 _ is1 is2 = dc trivial solve split combine $ D (is1,is2)
--  simple DC skeleton, no explicit placement, with depth control
karat 1 depth is1 is2 = parDC depth trivial solve split combine $ D (is1,is2)
-- karat 2 depth is1 is2 = dcN_c 3 depth trivial solve split combine  $ D (is1,is2)
-- karat 3 depth is1 is2 = dcNTickets_c 3 tickets trivial solve split combine $ D (is1,is2)
--  where tickets = [2..noPe]
karat 4 depth is1 is2 = flatDC farm depth trivial solve split combine $ D (is1,is2)
-- karat 5 depth is1 is2 = divConFlat_c map_ssf depth trivial solve split combine $ D (is1,is2)

-- karat _ _ _ _ = D [] -- error...

-- trivial stops rekursion when one MyInteger has less then 10 elements
trivial :: D (MyInteger,MyInteger) -> Bool
trivial  (D(is1,is2)) = lmin <= 10
 where lmin = min (length is1) (length is2)

-- split creates 3 subtasks of tuples(first halves, second halves, sum of first integer halves and sum of second integer halves) which are each multiplied in rekursive steps
split :: D (MyInteger,MyInteger) -> [D (MyInteger,MyInteger)]
split (D (is1,is2)) = [D(is1a,is2a),D(is1b,is2b),D(sumSeq is1a is1b,sumSeq is2a is2b)]
 where l1 = length is1
       l2 = length is2
       lmax = max l1 l2
       ldiv = lmax `div` 2 
       (is1b,is1a) = splitAt ldiv is1
       (is2b,is2a) = splitAt ldiv is2

-- combines the three subintegers (subtasks) using the length of the original MyIntegers
combine :: D (MyInteger,MyInteger) -> [D MyInteger] -> D MyInteger
combine (D(is1,is2)) [D u,D v,D w] = D result
 where l1 = length is1
       l2 = length is2
       lmax = max l1 l2
       ldiv = lmax `div` 2 
       u0s = replicate (2*ldiv) 0 ++ u -- u * base^(2*ldiv)
       (wuv,wuvS) = resSeqS (w,True) ((sumSeq u v),True) --calculate w-u-v (= w-(u+v))
       wuv0s = (replicate ldiv 0 ++ wuv,wuvS) -- multiply  w-u-v by base^ldiv
       result = fst (sumSeqS (sumSeq u0s v,True) wuv0s) -- sum of subresults (u * base^(2*ldiv) + (w-u-v)*base^ldiv + v) 

-- Multiply two arbitrary precision integers
solve :: D (MyInteger,MyInteger) -> D MyInteger
solve (D(is1,is2)) = D(multSeq isSmall isBig)
 where l1 = length is1
       l2 = length is2
       (isSmall,isBig) = if l1 > l2 then (is2,is1) else (is1,is2)     


-- Multiplying two arbitrary precision integers (the first has to be shorter)
multSeq :: MyInteger ->  MyInteger -> MyInteger     
multSeq isSmall isBig = foldl sumSeq [] iss0s
 where iss = [multEsc i isBig | i <- isSmall]
       iss0s = zipWith (++) [replicate n 0| n <- [0..]]  iss

-- Multiplying an arbitrary precision integer at every possition with an Int
multEsc :: Int -> MyInteger -> MyInteger
multEsc i is = normalize (map (i*) is)

-- apply the add carry
normalize :: MyInteger -> MyInteger
normalize is = noZeros (normalize' 0 is)

-- drop zeros at the end
noZeros :: MyInteger -> MyInteger
noZeros is = reverse (dropWhile (==0) (reverse is))

-- apply the add carry
normalize' :: Int -> MyInteger -> MyInteger
normalize' 0 [] = []
normalize' i [] = [i]
normalize' i (i1:is) = isum:normalize' iacc is
 where itot = i + i1
       isum = itot `mod` base
       iacc = itot `div` base
           
                 

-- Adding two arbitrary precision integers
sumSeq :: MyInteger -> MyInteger -> MyInteger
sumSeq is1 is2 = normalize $ zipWith (+) is1' is2'
 where (is1',is2') | (length is1 > length is2) = (is1,is2++repeat 0)
                   | otherwise                 = (is1++repeat 0,is2)
           
-- Subtracting two arbitrary precision integers
resSeq :: MyInteger -> MyInteger -> MyInteger
resSeq is1 is2 = normalize $ zipWith (-) is1 (is2++repeat 0)

-- Adding two arbitrary precision integers with sign
sumSeqS :: MyIntegerS -> MyIntegerS -> MyIntegerS
sumSeqS (is1,b1) (is2,b2)
 | b1==b2 = (sumSeq is1 is2,b1)
 | otherwise = (resSeq is1 is2,b1)

-- Subtracting two arbitrary precision integers with sign
resSeqS :: MyIntegerS -> MyIntegerS -> MyIntegerS
resSeqS is1S (is2,b2) = sumSeqS is1S (is2,not b2)