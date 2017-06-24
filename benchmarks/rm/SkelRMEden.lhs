Paralleles Rabin-Miller. Jetzt mit map+reduce Skelett.

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
module Main where

import RabinMiller
import FarmUntil

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Debug.Trace (trace)
import Text.Printf (printf)
\end{code}
\begin{code}
import Control.Parallel.Eden.Map (farmS)
import FarmUntil (mapUntil)
import Control.Parallel.Eden (noPe)

\end{code}
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
\begin{code}
-- | Pruefe ob n Primzahl        
rabinMillerPIO         :: Integer            -- ^ n ist der Primzahlkandidat
                   -> Int 
                   -> IO (Maybe Integer) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPIO n nTests = do
  ls <- randomBaseList n
  let s = listRabinMillerP nTests n ls 
  return s

-- | Pruefe ob n Primzahl
-- | HAAAACK!
rabinMillerP     :: Integer -- ^ n ist der Primzahlkandidat
                -> Int 
		-> Maybe Integer -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: moeglicherweise Prim)
rabinMillerP n nTests = unsafePerformIO $ rabinMillerPIO n nTests
\end{code}

Testen.
\begin{code}
createCandidate k = 2^k-1
doIt = rabinMillerP
main = do
  [k,nTests] <- getArgs
  let n = createCandidate $ read k
      res = doIt n (read nTests)
  putStrLn $ "Testing " ++ show n
  putStrLn $ "Working " ++  " at " ++ show noPe ++ " PEs."
  print res
  putStrLn "done"
\end{code}
