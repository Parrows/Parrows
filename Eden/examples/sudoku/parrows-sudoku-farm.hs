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
    [f,n] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (farm' (read n) solve grids)))

farm' :: NFData b => Int -> (a -> b) -> [a] -> [b]
farm' = farm ()