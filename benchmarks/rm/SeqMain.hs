module Main where

import RabinMiller
import System (getArgs)

main = do
  args <- getArgs
  let k :: Integer
      k = read $ head args
      n :: Integer
      n = 2^k-1
  print n
  res <- rabinMillerIO n
  print res
 
