import System (getArgs)
import Data.List

ll n m = take n $ repeat [1..m]

w = foldl1 intersect

main = do
  args <- getArgs
  let n = read $ head args
      m = read $ (args!!1)
      task = w $ ll n m
  print task
