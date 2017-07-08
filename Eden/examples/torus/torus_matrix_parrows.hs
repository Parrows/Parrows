{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
MultiParamTypeClasses #-}

module Main where

import Parrows.Eden
import Parrows.Definition
import Parrows.Future
import Parrows.Util
import Parrows.Skeletons.Topology
import Parrows.Skeletons.Map
import Data.List

--import GHC.Conc

import Control.Arrow
import Control.DeepSeq

import Control.DeepSeq

import Control.Applicative
import Data.Functor
import Data.List.Split

import System.Environment

import System.Random

import Debug.Trace

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

toMatrix :: Int -> [Int] -> Matrix
toMatrix cnt randoms = chunksOf n $ take (matrixIntSize n) randoms
        where n = cnt

matrixIntSize :: Int -> Int
matrixIntSize n = n * n

splitMatrix :: Int -> Matrix -> [[Matrix]]
splitMatrix size matrix = map (transpose . map (chunksOf size)) $ chunksOf size $ matrix

combine :: [[Matrix]] -> [[Matrix]] -> [[(Matrix, Matrix)]]
combine a b = zipWith (\a b -> zipWith (,) a b) a b

type MatrixExploded = [[[Matrix]]]

matMultTorus :: Int -> Int -> Matrix -> Matrix -> MatrixExploded
matMultTorus nodeCountVal problemSizeVal a b =
    let combined = combine (splitMatrix (problemSizeVal `div` nodeCountVal) a) (splitMatrix (problemSizeVal `div` nodeCountVal) b)
    in torus () (nodefunction nodeCountVal) $ combined

main = do
        args <- getArgs
        let (nodeCount:numCoresStr:problemSize:listSize:rest) = args
        let nodeCountVal = read nodeCount
        let problemSizeVal = read problemSize
        let listSizeVal = read listSize
        let numCores = read numCoresStr
        let aMatrices = take listSizeVal $ map (toMatrix problemSizeVal) $ (chunksOf (matrixIntSize problemSizeVal) randoms1)
        let bMatrices = take listSizeVal $ map (toMatrix problemSizeVal) $ (chunksOf (matrixIntSize problemSizeVal) randoms2)

        let cMatricesExploded = farm () numCores (uncurry $ matMultTorus nodeCountVal problemSizeVal) $ zipWith (,) aMatrices bMatrices
        print $ length $ (rnf cMatricesExploded) `seq` cMatricesExploded