module Main where

import Parrows.Definition
import Parrows.Future
import Parrows.Skeletons.Topology
import Parrows.Eden
import Data.List

import Control.DeepSeq

import Control.Applicative
import Data.Functor
import Data.List.Split

type Vector = [Int]
type Matrix = [Vector]

dimX :: Matrix -> Int
dimX = length

dimY :: Matrix -> Int
dimY = length . head

valAt :: Int -> [a] -> a
valAt pos [] = error $ show pos
valAt 0 (x:_) = x
valAt pos (_:xs) = valAt (pos - 1) xs

row :: Int -> Matrix -> Vector
row y = foldr (\x' y' -> valAt y x' : y') []

rows :: Matrix -> [Vector]
rows matrix = reverse $ go (dimY matrix - 1) matrix
    where
        go _ [] = []
        go 0 matrix = [row 0 matrix]
        go rowNum matrix = row rowNum matrix:go (rowNum - 1) matrix

matMult :: Matrix -> Matrix -> Matrix
matMult x y
    | dimX x /= dimY y = error "dimX x not equal to dimY y"
    | otherwise = chunksOf (dimX y) $ map sum (zipWith (*) <$> rows x <*> y)

matAdd :: Matrix -> Matrix -> Matrix
matAdd x y
    | dimX x /= dimX y = error "dimX x not equal to dimX y"
    | dimY x /= dimY y = error "dimY x not equal to dimY y"
    | otherwise = chunksOf (dimX x) $ zipWith (+) (concat x) (concat y)

-- from: http://www.mathematik.uni-marburg.de/~eden/paper/edenCEFP.pdf (page 38)
nodefunction :: Int                         -- torus dimension
    -> ((Matrix, Matrix), [Matrix], [Matrix]) -- process input
    -> ([Matrix], [Matrix], [Matrix])       -- process output
nodefunction n ((bA, bB), rows, cols )
    = ([bSum], bA:nextAs , bB:nextBs )
    where bSum = foldl' matAdd (matMult bA bB) (zipWith matMult nextAs nextBs)
          nextAs = take (n-1) rows
          nextBs = take (n-1) cols

testMatrix :: Matrix
testMatrix = replicate 10 [1..10]

main = print $ (rnf val) `seq` val
    where
        val = torus () (nodefunction 8) [[(testMatrix, testMatrix)]]