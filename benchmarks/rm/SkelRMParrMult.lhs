Paralleles Rabin-Miller. Jetzt mit map+reduce Skelett.

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
module Main where

import RabinMiller

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Debug.Trace (trace)
import Text.Printf (printf)
\end{code}
\begin{code}
import Parrows.Definition
import Parrows.Multicore.Simple
import Parrows.Skeletons.Map

mapUntil :: ((a -> b) -> [a] -> [b]) ->    -- ^ map
            ([b] -> c) ->                  -- ^ reduce
            (a -> b) ->                    -- ^ worker function
            [a] -> c
mapUntil amap areduce f xs = areduce $ amap f xs -- meh.


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
-- new skeletal code
\begin{code}
listRabinMillerP2 :: Integer        -- ^ n ist der Primzahlkandidat
                  -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                  -> Int        -- ^ Restliche Durchlaeufe
                  -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl)
listRabinMillerP2 n as k = let f :: (Integer, Integer) -> Bool
                               f (n, a) = singleRabinMillerBool n t 0 b
                                   where ((q,t),b) = (zerlege(n-1,0) , powermod a q n)
                               tasks = take k $ [(n, a) | a<-as]
                               reduce :: Integer -> [Bool] -> Maybe Integer
                               reduce n bs | and bs = Just n
                                           | otherwise = Nothing
                           in mapUntil (\fn tsks -> farm () 512 fn $ tsks) (reduce n) f tasks
\end{code}
Sequentielles Code.
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
  putStrLn $ "nTests " ++ nTests
  print res
  putStrLn "done"
\end{code}
