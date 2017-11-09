Sequentielles Rabin-Miller Programm

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
module Main where

import RabinMiller
--import FarmUntil

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Debug.Trace (trace)
import Text.Printf (printf)
\end{code}
\begin{code}
--import Control.Parallel.Eden.Map (farmS)
--import FarmUntil (mapUntil)
--import Control.Parallel.Eden (noPe)

-- Autor: Kent Kwee
-- [...]
-- | Rabin-Miller
-- | Fuehre 20 Iterationen durch für Fehlerwahrscheinlichkeit < 0.25 ^ 20
listRabinMillerSeq      :: Integer         -- ^ n ist der Primzahlkandidat
                    -> [Integer]       -- ^ die Liste mit den Basen fuer RabinMiller
                    -> Integer
                    ->  Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerSeq n as nTests
      | (n<3)            = error "N must be greater than 3."
      | (n `mod` 2 == 0) = error "N must be odd."  
      | otherwise        = listRabinMillerSeq2 n as nTests
\end{code}
\begin{code}
-- | RabinMiller für feste Basen a aus einer Integerliste
listRabinMillerSeq2 :: Integer        -- ^ n ist der Primzahlkandidat
                -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                -> Integer        -- ^ Restliche Durchlaeufe
                -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerSeq2 n (a:as) c
     | (c==0)                          = Just n
     | (singleRabinMillerBool n t 0 b) = listRabinMillerSeq2 n as (c-1) 
     | otherwise                       = Nothing
     where ((q,t),b) = (zerlege(n-1,0) , powermod a q n)    
\end{code}
Sequentielles Code.
\begin{code}
-- | Pruefe ob n Primzahl        
rabinMillerPIO         :: Integer            -- ^ n ist der Primzahlkandidat
                   -> Integer 
                   -> IO (Maybe Integer) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPIO n nTests = do
  ls <- randomBaseList n
  let s = listRabinMillerSeq n ls nTests
  return s

-- | Pruefe ob n Primzahl
-- | HAAAACK!
rabinMillerP     :: Integer -- ^ n ist der Primzahlkandidat
                -> Integer 
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
  --putStrLn $ "Working " ++  " at " ++ show noPe ++ " PEs."
  print res
  putStrLn "done"
\end{code}
