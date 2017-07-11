{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Parrows.Definition
import Parrows.Skeletons.Map
import Parrows.Eden
import Control.Parallel.Eden
import Control.DeepSeq

import GHC.Arr

instance Trans ((GHC.Arr.Array Sudoku.Square [Sudoku.Digit]))

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (parMap () solve grids)))
