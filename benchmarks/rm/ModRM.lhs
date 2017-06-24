Paralleles Rabin-Miller. Jetzt mit map+reduce Skelett.

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
{-# OPTIONS -XTypeSynonymInstances #-}
module Main where

import ModRabinMiller

import System.IO.Unsafe (unsafePerformIO)
import System (getArgs)

import Debug.Trace (trace)
import Text.Printf (printf)
\end{code}
\begin{code}
import Parallel.Skel.EdenSkel (map_farm)
import Parallel.Eden (noPe, rnf, Trans, NFData)
\end{code}
\begin{code}
import MathObj.Residue.IntMulti
import MathObj.Residue.Modulo
import MathObj.Primes.Determ
import Data.Maybe

import ModRabinMiller
\end{code}

\begin{code}
instance NFData MyInt
instance Trans MyInt

hasNothing :: [Maybe a] -> Bool
hasNothing = not . and . map isJust

restoreIZmb :: [Maybe (Mod Int)] -> Maybe (Mod Integer)
restoreIZmb xs | hasNothing xs = Nothing
               | otherwise = let ys = catMaybes xs
                             in Just $ restoreIZinteger ys

createCandidate k = 2^k-1

doItSeq n rs = restoreIZmb $ unsafePerformIO $ mapM rabinMillerIO $ makeIZ'' n rs
doItPar n rs = restoreIZmb $ map_farm rabinMiller $ makeIZ'' n rs

doIt :: Int -> Integer -> [Int] -> Maybe (Mod Integer)
doIt 1 = doItSeq
doIt 2 = doItPar
doIt _ = error "Don't know what version to use"

main = do
  args <- getArgs
  let t = if length args < 4 then 1
          else read $ head args :: Int
      k = if length args < 4 then 5
          else read (args!!1)
      l = if length args < 4 then 1
          else read (args!!2)
      r = if length args < 4 then 23
          else read (args!!3)
  let n = createCandidate k :: Integer
      rs = map (fromIntegral) $ take l $ primesFrom r
      res = doIt t n rs
  putStrLn $ "Testing " ++ show n
  putStrLn $ "Using " ++ show l ++ " residues, beginning from " ++ show r
  -- putStr "Generating residues... "
  rnf rs `seq` putStrLn "done"
  putStrLn $ "Working " ++  " at " ++ show noPe ++ " PEs."
  print res
  putStrLn "done"
\end{code}
