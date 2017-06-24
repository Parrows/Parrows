Paralleles Rabin-Miller

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
-- module ParRM where
module Main where

import RabinMiller
import MintUntil
import VanillaUntil (iterUntil, mwUntil)

import System.IO.Unsafe (unsafePerformIO)
import System (getArgs)

import Parallel.Skel.EdenSkel (map_par, map_farm)
import Parallel.Eden (noPe)

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

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
listRabinMillerP :: Int -- ^ Skeletttyp
                 -> Int             -- ^ k ist die Anzahl der Tests k
                 -> Integer         -- ^ n ist der Primzahlkandidat
                 -> [Integer]       -- ^ die Liste mit den Basen fuer RabinMiller
                 ->  Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerP t k n as
      | (n<3)            = error "N must be greater than 3."
      | (n `mod` 2 == 0) = error "N must be odd."  
      | otherwise        = listRabinMillerP2' t n as k
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
\begin{codeold}
listRabinMillerP2Old :: Integer        -- ^ n ist der Primzahlkandidat
                  -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                  -> Int            -- ^ Anzahl Durchlaeufe
                  -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
\end{codeold}
\begin{code}
listRabinMillerP2' :: Int -- ^ Skelettyp
                  -> Integer        -- ^ n ist der Primzahlkandidat
                  -> [Integer]      -- ^ die Liste mit den Basen fuer RabinMiller
                  -> Int            -- ^ Anzahl Durchlaeufe
                  -> Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMillerP2' 2 n xs k | res = Just n
                            | otherwise = Nothing
    where res = stateIter createTask2 worker2 and (n, xs)
listRabinMillerP2' 0 n xs k = undefined
--    = iterUntil (n, k, xs) createTask1 worker1 doneContinue1
listRabinMillerP2' 1 n xs k 
    = mwUntil (n, k, xs) createTaskAll worker1 doneContinueAll
createTask1 (n, k, xs) = take nope [(i, n, x) | x<-xs, i<-[k-nope..k] ]
createTaskAll (n, k, xs) = [(i, n, x) | x<-xs, i<-[k-nope..k] ]
worker1 :: (Int, Integer, Integer) -> (Bool, Int)
worker1 (k, n, x) = (singleRabinMillerBool n t 0 b, k)
    where ((q,t),b) = (zerlege(n-1,0) , powermod x q n)    
-- | doneContinue bekommt den Primzahlkandidaten und die Liste von Bools
-- | ist wenigstens ein Eintrag in der Liste False, dann ist es keine Primzahl
-- | sind sie alle True -- es ist eine moegliche Primzahl
doneContinue1 (n, _, xs) bks | not $ and bs = Left Nothing
                            | k<=0 = Left (Just n)
                            | otherwise = Right (createTask1 (n, k-length bs, xs))
    where (bs, ks) = unzip bks
          k = minimum ks
doneContinueAll (n, _, xs) bks | not $ and bs = Left Nothing
                               | k<=0 = Left (Just n)
                               | otherwise = error "Needed to create more tasks!"
    where (bs, ks) = unzip bks
          k = minimum ks
  
\end{code}
\begin{skeleton}
stateIter :: forall bigTask task res bigRes.
            ((Trans bigTask, Trans task, 
              Trans res, Trans bigRes, NFData res) =>
             (bigTask -> [task]) -> 
             (task -> Maybe res) -> 
             ([res] -> bigRes) -> 
             bigTask -> bigRes)
\end{skeleton}
\begin{code}
createTask2 (n, xs) = trace "Creating tasks..." $
                     take 20 [(n, x) | x<-xs]
worker2 :: (Integer, Integer) -> Maybe Bool -- Nothing oder Just True
worker2 (n, x) | res = Just True
               | otherwise = Nothing -- results in an abort!
    where ((q,t),b) = (zerlege(n-1,0) , powermod x q n)    
          res = trace "A little step for a PE..." $ 
                singleRabinMillerBool n t 0 b
      
-- | doneContinue bekommt den Primzahlkandidaten und die Liste von Bools
-- | ist wenigstens ein Eintrag in der Liste False, dann ist es keine Primzahl
-- | sind sie alle True -- es ist eine moegliche Primzahl
\end{code}

wir brauchen 20 iterationen und wollen alle pes auslasten
\begin{codez}
getPe = nope * ( 20`div`nope + 1)
nope = max noPe 1
\end{codez}
\begin{code}
getPe = 20
nope = max noPe 1
-- noPe = 1
\end{code}
Sequentielles Code.
\begin{code}
-- | Pruefe ob n Primzahl        
rabinMillerPIO :: Int -- ^ Skelettyp
               -> Integer            -- ^ n ist der Primzahlkandidat
               -> IO (Maybe Integer) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPIO t n = do
  ls <- randomBaseList n
  let s = listRabinMillerP t getPe n ls 
  return s

-- | Pruefe ob n Primzahl
-- | HAAAACK!
rabinMillerP     :: Int -- ^ Skelettyp
                 -> Integer -- ^ n ist der Primzahlkandidat
		-> Maybe Integer -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: moeglicherweise Prim)
rabinMillerP t n = unsafePerformIO $ rabinMillerPIO t n 
\end{code}

Testen.
\begin{code}
createCandidate k = 2^k-1
doIt = rabinMillerP
main = do
  args <- getArgs
  let t = read $ head args
      k = read $ args!!1
      n = createCandidate k
      res = doIt t n
  putStrLn "<typ> <input length, power of 2>"
  putStrLn "0 - vanillaUntil with parmap (BROKEN)"
  putStrLn "1 - vanillaUntil with mw (CAUTION!)"
  putStrLn "1 - mintUntil with stateful mw"
  putStrLn $ "Testing " ++ show n
  putStrLn $ "Working " ++  show getPe ++ " iterations at " ++ show noPe ++ " PEs."
  print res
  putStrLn "done"
\end{code}