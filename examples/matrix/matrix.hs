-- Matrix Multiplication is a parMap

import ParallelSplit.Definition
import ParallelSplit.Multicore
import Data.List.Split

type Scalar = Float
type Vector = [Scalar]
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

matrixP :: Matrix -> Matrix -> Matrix
matrixP x y
    | dimX x /= dimY y = error "dimX x not equal to dimY y"
    | otherwise = chunksOf (dimX y) $ parMap sum (zipWith (*) <$> rows x <*> y)

testMatrix :: Matrix
testMatrix = replicate 100 [1..100]

main :: IO ()
main = print ("done" ++ (show (length (matrixP testMatrix testMatrix))))
