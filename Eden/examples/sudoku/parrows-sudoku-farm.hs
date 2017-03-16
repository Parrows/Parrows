{-# LANGUAGE FlexibleContexts #-}
import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Parrows.Definition
import Parrows.Skeletons.Map
import Parrows.Eden
import Control.DeepSeq

import Control.Parallel.Eden

main :: IO ()
main = do
    [f,n] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (farm' (read n) solve grids)))

farm' :: (Trans a, Trans b) => Int -> (a -> b) -> [a] -> [b]
farm' = farm ()