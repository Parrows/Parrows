{-# OPTIONS -cpp #-}
module Main where

#define TEST 1

import JacobiSum
import ModArithmetik (primelist)
import Test.QuickCheck

-- TODO: replace with trial division?
isReallyPrime :: Integer -> Bool
isReallyPrime n | n>0 = let primes = primelist n -- ^ it contains all primes including n, if n is prime
                            res = filter (==n) $ reverse primes -- ^ [n] falls ja, [] falls nein
                            decide [] = False
                            decide [n] = True
                            decide _  = False
                        in decide res
                | n<=0 = False


testJacobi n | n>0  = jacobisumtest n (bestimme_t n)
             | n<=0 = False

prop_JacobiWorks n = (testJacobi $ fromIntegral n) == (isReallyPrime $ fromIntegral n)
    where types = n :: Int

main = quickCheck prop_JacobiWorks
