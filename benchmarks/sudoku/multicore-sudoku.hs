import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.Parallel.Strategies

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (parMap rdeepseq solve grids)))
