Paralleles Rabin-Miller

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
-- module ParRM where
module Main where

import RabinMiller
import FarmUntil612

import System.IO.Unsafe (unsafePerformIO)
import System (getArgs)

import Debug.Trace (trace)
import Text.Printf (printf)
\end{code}
\begin{code}
import Control.Parallel.Eden.EdenSkel.MapSkels (map_par, map_farm)
import Control.Parallel.Eden (noPe)
\end{code}
\begin{seqcode}
module RabinMiller where 
-- Autor: Kent Kwee
-- [...]
-- | Rabin-Miller
-- | Fuehre 20 Iterationen durch für Fehlerwahrscheinlichkeit < 0.25 ^ 20
listRabinMiller      :: Integer         -- ^ n ist der Primzahlkandidat
                    -> [Integer]       -- ^ die Liste mit den Basen fuer RabinMiller
                    ->  Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMiller n as
      | (n<3)            = error "N must be greater than 3."
      | (n `mod` 2 == 0) = error "N must be odd."  
      | otherwise        = listRabinMiller2 n as 20
\end{seqcode}
\begin{code}
-- | Rabin-Miller
-- | Fuehre 20 Iterationen durch für Fehlerwahrscheinlichkeit < 0.25 ^ k
listRabinMillerP :: Int             -- ^ k ist die Anzahl der Tests k
                 -> Integer         -- ^ n ist der Primzahlkandidat
                 -> [Integer]       -- ^ die Liste mit den Basen fuer RabinMiller
                 ->  Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerP k n as
      | (n<3)            = error "N must be greater than 3."
      | (n `mod` 2 == 0) = error "N must be odd."  
      | otherwise        = listRabinMillerP2 n as k
\end{code}
\begin{seqcode}
-- | RabinMiller für feste Basen a aus einer Integerliste
listRabinMiller2 :: Integer        -- ^ n ist der Primzahlkandidat
                -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                -> Integer        -- ^ Restliche Durchlaeufe
                -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMiller2 n (a:as) c
     | (c==0)                          = Just n
     | (singleRabinMillerBool n t 0 b) = listRabinMiller2 n as (c-1) 
     | otherwise                       = Nothing
     where ((q,t),b) = (zerlege(n-1,0) , powermod a q n)    
\end{seqcode}
\begin{code}
listRabinMillerP2 :: Integer        -- ^ n ist der Primzahlkandidat
                  -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                  -> Int        -- ^ Restliche Durchlaeufe
                  -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerP2 n as k = let f :: (Integer, Integer) -> Bool
                               f (n, a) = singleRabinMillerBool n t 0 b
                                   where ((q,t),b) = (zerlege(n-1,0) , powermod a q n)
                               tasks = take k $ [(n, a) | a<-as]
                               res = trace ("Need to do " ++ (show k) ++ " iterations, " 
                                     ++ "have data for " ++ (show $ length tasks) ) $ 
                                     farmUntilBool f tasks
                           in case res of
                                True -> Just n
                                False -> Nothing
\end{code}
Sequentielles Code.
\begin{code}
-- | Pruefe ob n Primzahl        
rabinMillerPIO         :: Integer            -- ^ n ist der Primzahlkandidat
                   -> IO (Maybe Integer) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPIO n = do
  ls <- randomBaseList n
  let s = listRabinMillerP 20 n ls 
  return s

-- | Pruefe ob n Primzahl
-- | HAAAACK!
rabinMillerP     :: Integer -- ^ n ist der Primzahlkandidat
		-> Maybe Integer -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: moeglicherweise Prim)
rabinMillerP n = unsafePerformIO $ rabinMillerPIO n
\end{code}

Testen.
\begin{code}
createCandidate k = 2^k-1
doIt = rabinMillerP
main = do
  args <- getArgs
  let k = read $ head args
      n = createCandidate k
      res = doIt n
  putStrLn $ "Testing " ++ show n
  putStrLn $ "Working " ++  " at " ++ show noPe ++ " PEs."
  print res
  putStrLn "done"
\end{code}
