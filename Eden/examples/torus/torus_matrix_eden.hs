module Main where

import Parrows.Definition
import Parrows.Future
import Control.Parallel.Eden.Topology
--import Parrows.Skeletons.Topology
import Parrows.Eden
import Data.List

import Control.Applicative
import Data.Functor
import Data.List.Split

import Control.DeepSeq

import System.Random

type Vector = [Int]
type Matrix = [Vector]

dimX :: Matrix -> Int
dimX = length

dimY :: Matrix -> Int
dimY = length . head

--from: https://rosettacode.org/wiki/Matrix_multiplication#Haskell
foldlZipWith::(a -> b -> c) -> (d -> c -> d) -> d -> [a] -> [b]  -> d
foldlZipWith _ _ u [] _          = u
foldlZipWith _ _ u _ []          = u
foldlZipWith f g u (x:xs) (y:ys) = foldlZipWith f g (g u (f x y)) xs ys

foldl1ZipWith::(a -> b -> c) -> (c -> c -> c) -> [a] -> [b] -> c
foldl1ZipWith _ _ [] _          = error "First list is empty"
foldl1ZipWith _ _ _ []          = error "Second list is empty"
foldl1ZipWith f g (x:xs) (y:ys) = foldlZipWith f g (f x y) xs ys

multAdd::(a -> b -> c) -> (c -> c -> c) -> [[a]] -> [[b]] -> [[c]]
multAdd f g xs ys = map (\us -> foldl1ZipWith (\u vs -> map (f u) vs) (zipWith g) us ys) xs

matMult:: Num a => [[a]] -> [[a]] -> [[a]]
matMult xs ys = multAdd (*) (+) xs ys

matAdd :: Matrix -> Matrix -> Matrix
matAdd x y
    | dimX x /= dimX y = error "dimX x not equal to dimX y"
    | dimY x /= dimY y = error "dimY x not equal to dimY y"
    | otherwise = chunksOf (dimX x) $ zipWith (+) (concat x) (concat y)

-- from: http://www.mathematik.uni-marburg.de/~eden/paper/edenCEFP.pdf (page 38)
nodefunction :: Int                         -- torus dimension
    -> ((Matrix, Matrix), [Matrix], [Matrix]) -- process input
    -> ([Matrix], [Matrix], [Matrix])       -- process output
nodefunction n ((bA, bB), rows, cols)
    = ([bSum], bA:nextAs , bB:nextBs)
    where bSum = foldl' matAdd (matMult bA bB) (zipWith matMult nextAs nextBs)
          nextAs = take (n-1) rows
          nextBs = take (n-1) cols

randoms1 :: [Int]
randoms1 = randoms $ mkStdGen 23586

randoms2 :: [Int]
randoms2 = randoms $ mkStdGen 67123

factor :: Int
factor = 1

n :: Int
n = 32 * factor

aMatrix :: Matrix
aMatrix = chunksOf n $ take (n * n) randoms1

bMatrix :: Matrix
bMatrix = chunksOf n $ take (n * n) randoms2

splitMatrix :: Int -> Matrix -> [[Matrix]]
splitMatrix size matrix = map (transpose . map (chunksOf size)) $ chunksOf size $ matrix

combine :: [[Matrix]] -> [[Matrix]] -> [[(Matrix, Matrix)]]
combine a b = zipWith (\a b -> zipWith (,) a b) a b

main = print $ length $ (rnf val) `seq` val
    where
        val = torus (\ x y z -> nodefunction 16 (x, y, z)) $ combine (splitMatrix (2 * factor) aMatrix) (splitMatrix (2 * factor) bMatrix)
