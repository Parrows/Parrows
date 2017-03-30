module Main where

import Parrows.Definition
import Parrows.Future
import Parrows.Util
import Parrows.Skeletons.Topology

import Parrows.Eden

import Control.Parallel.Eden
import Control.Parallel.Eden.Topology

import Control.DeepSeq

import Debug.Trace

ring_iterate :: Int -> Int -> Int -> ([Int], [[Int]]) -> ([Int], [[Int]])
ring_iterate size k i (rowk, (rowi:xs))
    | i > size = (rowk, []) -- Finish Iteration
    | i == k = (rowR, rowk:restoutput) -- send own row
    | otherwise = (rowR, rowi:restoutput) -- update row
    where
    (rowR, restoutput) = rnf nextrowk `pseq`
        ring_iterate size k (i+1) (nextrowk, xs)
    nextrowk
        | i == k = rowk -- no update , if own row
        | otherwise = updaterow rowk rowi (rowk !! (i-1))

updaterow rowk rowi elem = trace (show rowk ++ show rowi ++ show elem) $ rowk

adjacency :: [[Int]]
adjacency = [[0, 1, 2],
             [1, 0, 2],
             [1, 2, 0]]

main = print $ ringSimple (curry $ ring_iterate 3 1 1) adjacency