{-# LANGUAGE FlexibleInstances #-}
import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.Parallel.Eden.Map
import Control.Parallel.Eden(Trans)
import GHC.Arr(Array)

instance Trans (GHC.Arr.Array Square [Digit])

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (parMap solve grids)))
