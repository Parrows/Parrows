module Main where

import Control.DeepSeq
import ParallelSplit.Definition
import ParallelSplit.ParMonad

-- TODO: remove duplicated code

-- idea lazily copied from:
-- https://gist.github.com/morae/8494016

fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge fn (x:xs) (y:ys)
      | fn x y == LT = x:merge fn xs (y:ys)
      | fn x y == EQ = x:merge fn xs (y:ys)
      | otherwise = y:merge fn (x:xs) ys

-- this is our multithreaded function
-- it spawns 2 threads that do the mergesort
mergesortPar :: (NFData a) => (a -> a -> Ordering) -> [a] -> [a]
mergesortPar fn xs
  | length xs <= 1 = xs
  -- we get multithreading for zero readability cost here
  -- first we split the sorting process into two different processes <|||>
  -- and then we merge them with <&&&>
  | otherwise = (mergesort fn <|||> mergesort fn <&&&> merge fn) (fsthalf xs, sndhalf xs)

mergesort :: (a -> a -> Ordering) -> [a] -> [a]
mergesort fn xs
    | length xs <= 1 = xs
    | otherwise = merge fn (mergesort fn (fsthalf xs)) (mergesort fn (sndhalf xs))

stuff :: [Integer]
stuff = [1..999999]

order x y
    | x > y = GT
    | x < y = LT
    | x == y = EQ

main = print $ mergesortPar order stuff
