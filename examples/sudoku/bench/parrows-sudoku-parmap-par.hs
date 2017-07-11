{-# LANGUAGE FlexibleContexts #-}
import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Parrows.Definition
import Parrows.Skeletons.Map
import Parrows.ParMonad
import Control.DeepSeq

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (parMap () solve grids)))
