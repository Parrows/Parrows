{-# LANGUAGE FlexibleContexts #-}
import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Parrows.Definition
import Parrows.ParMonad
import Parrows.Skeletons.Map
import Control.DeepSeq

main :: IO ()
main = do
    [f,n] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (parMapChunk (read n) solve grids)))

parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> [b]
parMapChunk = parMapStream ()