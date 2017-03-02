{-# LANGUAGE FlexibleContexts #-}
import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Parrows.Definition
import Parrows.ParMonad
import Control.DeepSeq

main :: IO ()
main = do
    [f,chunkSize, cores] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (farmChunk' (read chunkSize) (read cores) solve grids)))

farmChunk' :: NFData b => Int -> Int -> (a -> b) -> [a] -> [b]
farmChunk' = farmChunk ()