module Main where

import JacobiSum 
import System (getArgs)

-- Tester

main = do
 -- args <- getArgs
 -- let n = read $ head args
 --     t = if (length args > 1)
 --         then read $ args!!1
 --         else bestimme_t n
 args <- getArgs
 let k = read $ head args
     n | length args < 3 =  k
       | read (args!!2) = 2^k-1
       | otherwise = k
     t | length args > 1 && read (args!!1) > 0 = read (args!!1)
       | otherwise = bestimme_t n
 putStrLn $ "Jacobi Sum Test.\nn = " ++ (show n) ++ ", t = " ++ (show t)
 print $ jacobisumtest n t
 putStrLn "done"
