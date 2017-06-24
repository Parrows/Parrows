Paralleles Rabin-Miller

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
-- module ParRM where
module Main where

import RabinMiller
import VanillaUntil

import System.IO.Unsafe (unsafePerformIO)
import System (getArgs)

import Debug.Trace (trace)
import Text.Printf (printf)
\end{code}
\begin{code}
import Control.Parallel
import Control.Parallel.Strategies
import GHC.Conc (numCapabilities)
import Data.List (transpose)

parmap f xs = map f xs `using` parList rnf -- ^ unbounded parmap
farm f xs = let tasks = unshuffleN nope xs
                worker = map f
            in shuffleN $ parmap worker tasks
\end{code}
\begin{code}
{- unshuffleN splits a list into n lists
    [takeEach n (drop i xs) | i <- [0..(n-1)]] -}
unshuffleN :: Int -> [a] -> [[a]]
unshuffleN n xs = unshuffle xs
		where  unshuffle xs = map (f xs) [0..n-1]
				where f xs i = g (drop i xs)
				      g [] = []
				      g xs = head xs : (g (drop n xs))

-- simple shuffling (not incremental!)
shuffle :: [[a]] -> [a]
shuffle = concat . transpose

{- 
shuffleN joins n lists which had been splitted with unshuffle
-}
shuffleN :: [[b]] -> [b]
-- shuffleN = concat . transpose 
-- this impl. sequential evaluation on input list after	the other
-- for Eden we need a version, that produces the first outputs as fast
--  as possible, i. e. evaluates all input lists concurrently:
shuffleN xxs 
	| and (map null xxs) = []
	| otherwise = (mymaphead xxs) ++ ( shuffleN (map mytail xxs))
		 where mymaphead [] = []
		       mymaphead ([]:xxs) = mymaphead xxs
		       mymaphead ((x:xs):xxs) = x : mymaphead xxs
		       mytail [] = []
		       mytail xs = tail xs
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
\begin{skeleton}
iterUntil :: ((task -> subResult) -> [task] -> [subResult]) -> -- ^ map implementation
             dataIn ->                                                   -- ^ input data
             (dataIn -> [task]) ->                   -- ^ splitter function to generate subtasks
             (task -> subResult) -> -- ^ single worker function
             (dataIn -> [subResult] -> Either result [task]) -> -- ^ combine result and
                                                      -- ^ determin whether we are done
             result -- ^ the combined result
\end{skeleton
\begin{code}
listRabinMillerP2 :: Integer        -- ^ n ist der Primzahlkandidat
                  -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                  -> Int            -- ^ Anzahl Durchlaeufe
                  -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerP2 n xs k = iterUntil parmap (n, k, xs) createTask worker doneContinue
createTask (n, k, xs) = take nope [(i, n, x) | x<-xs, i<-[k-nope..k] ]
worker :: (Int, Integer, Integer) -> (Bool, Int)
worker (k, n, x) = (singleRabinMillerBool n t 0 b, k)
    where ((q,t),b) = (zerlege(n-1,0) , powermod x q n)    
-- | doneContinue bekommt den Primzahlkandidaten und die Liste von Bools
-- | ist wenigstens ein Eintrag in der Liste False, dann ist es keine Primzahl
-- | sind sie alle True -- es ist eine moegliche Primzahl
doneContinue (n, _, xs) bks | not $ and bs = Left Nothing
                            | k<=0 = Left (Just n)
                            | otherwise = Right (createTask (n, k-length bs, xs))
    where (bs, ks) = unzip bks
          k = minimum ks
  
\end{code}

wir brauchen 20 iterationen und wollen alle pes auslasten
\begin{codez}
getPe = nope * ( 20`div`nope + 1)
nope = max noPe 1
\end{codez}
\begin{code}
getPe = 20
nope = max noPe 1
noPe = max 1 numCapabilities
\end{code}
Sequentielles Code.
\begin{code}
-- | Pruefe ob n Primzahl        
rabinMillerPIO         :: Integer            -- ^ n ist der Primzahlkandidat
                   -> IO (Maybe Integer) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPIO n = do
  ls <- randomBaseList n
  let s = listRabinMillerP getPe n ls 
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
  putStrLn $ "Working " ++  show getPe ++ " iterations at " ++ show noPe ++ " PEs."
  print res
  putStrLn "done"
\end{code}