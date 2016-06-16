-- Matrix Multiplication is a parMap

import Parrows.Definition
import Parrows.Multicore
import Data.List.Split
import Data.Maybe
import Control.Arrow

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
    | otherwise = chunksOf (dimX y) $ parMap (sum, (zipWith (*) <$> rows x <*> y))

instance MonadUnwrap Maybe where
    unwrap = fromJust

-- hacky
matrixPKleisli :: Kleisli (Maybe) (Matrix, Matrix) Vector
matrixPKleisli = (Kleisli $ (\(x, y) -> Just (zipWith (*) <$> rows x <*> y)))>>>
                 (tup (arr sum)) >>> parMap

testMatrix :: Matrix
testMatrix = replicate 1000 [1..1000]

main :: IO ()
main = print ("done" ++ (show (length (matrixP testMatrix testMatrix))))
--main = print ("done" ++ (show (length (fromJust (runKleisli matrixPKleisli (testMatrix, testMatrix))))))
