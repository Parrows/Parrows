Jacobi sums in parallel.

Simple workpool with reduce seq., now in a skeleton

\begin{code}
{-# OPTIONS -cpp #-}
#ifdef INTERACTIVE
module NewSkelJacobiSum where
#else 
module Main where
#endif
-- Autor: Kent Kwee
\end{code}

Modifications for parallelism by: Oleg Lobachev
\begin{code}
import ModArithmetik
import Ringerweiterung 
import System.Environment (getArgs)
import Data.Array

import JacobiSum hiding (jacobisumteststep3, jacobisumtest, jacobi)
import qualified JacobiSum as JS

import Data.Maybe
-- import MapHacks -- old verisons
import Parrows.Eden
import Parrows.Definition
import Parrows.Skeletons.Map
import Control.Parallel.Eden (noPe, Trans)
import FarmUntil
import Data.List (nub, sort, intersect)
import Control.DeepSeq
\end{code}

Replace MapHacks with up-to-date Eden Skeletons
\begin{code}

\end{code}

\begin{code}
-- import List (elemIndex)
-- import Debug.Trace (trace)
-- trace _ x = x
--- trace is imported
-- loud s x = trace (s ++ " " ++ show x) x
\end{code}
\begin{code}
jacobisumteststep3 :: Int -- ^ skeleton to use
                   -> [(Integer,Integer)] -- ^ (p,q) - Liste mit p^k || (q-1) | t
                 -> Integer             -- ^ n: zu pruefende Zahl
                 -> Integer             -- ^ t: Parameter t aus der Vorberechnung
                 -> [Integer]           -- ^ Liste von Primzahlen mit l_p = 0
                 -> Maybe [Integer]         -- ^ Das Ergebnis des Tests:
                                        --   Nothing: keine Primzahl
                                        --   Plist (Liste von Primzahlen mit l_p = 0)
\end{code}
Seq. code:
\begin{seq}
jacobisumteststep3 [] n t lpliste = trace ("In step 3: returning " ++ show lpliste)
                                    Just lpliste 
jacobisumteststep3 ((p,q):rest) n t lpliste 
          | (erg == Nothing) =  trace ("In step 3: tested " ++ show (p,q) ++ " and bailed out") $ 
                                Nothing
          | otherwise =  trace ("In step 3: tested " ++ show (p,q) ++ " and looping on") $ 
                         jacobisumteststep3 rest n t lpnew
          where k = nu (q-1) p  -- Vielfachheit von p in (q-1)
                erg = jacobisumteststep4 p k q n lpliste
                lpnew = getlist erg 
\end{seq}

A nice test call is:
jacobisumteststep3 [(2,3),(2,5),(2,7),(3,7),(2,13),(3,13)] 31 12 [2]

a script to test the whole thing:
for n in `seq 3 100`; do test  `./olJacobiPar $n +RTS -qp8 2>/dev/null | egrep -i 'true|false'` = `./olJacobiSeq $n 2>/dev/null | egrep -i 'true|false'` || (echo -n "FAIL: "; echo -n $n; echo -n ", ") ; done; echo "done";

an example, where the parallel version fails: 2083.

a new attempt with unify!
Unifying the lists of lists
\begin{code}
unify :: (Eq a, Ord a, Show a) => [[a]] -> [a]
unify = foldl1 intersect . nub -- nub gives better performance

unifyMaybe :: (Eq a, Ord a, Show a) => [Maybe [a]] -> Maybe [a]
unifyMaybe xss | hasNothing xss = trace "unifyMaybe: bailing out!" $ 
                                  Nothing
               | otherwise = Just $ unify $ catMaybes xss
    where hasNothing = not . all maybeBool
          maybeBool (Just _) = True
          maybeBool Nothing = False

jacobisumteststep3 0 xs n t lps = JS.jacobisumteststep3 xs n t lps
\end{code}

With new skeleton, using workpool
\begin{code}
jacobisumteststep3 m xs n t lps =
    let worker (p, q) = jacobisumteststep4 p k q n lps
            where k = nu (q-1) p  -- Vielfachheit von p in (q-1)
        mymap | m==1 = map
	      | m==2 = parMap ()
	      | m==3 = farm () noPe
	      -- | m==6 = map_wp
    in mapUntil mymap unifyMaybe worker xs

-- all other cases
-- jacobisumteststep3 _ _ _ _ _ = error "Not implemented"
\end{code}


Further seq. code. imported

\begin{code}
-- | Der Jacobisumtest                   
jacobisumtest :: Int -- ^ Skelett
              -> Integer  -- ^ zu pruefende Zahl n
              -> Integer  -- ^ der Parameter t aus der Vorberechnung
              -> Bool     -- ^ Ist n Primzahl? 
jacobisumtest s n t
       | (n < 2)  = False
       | (n == 2) = True
		| (n == 3) = True
		| (n == 5) = True
		| (n == 7) = True		
       -- rabinmillertest
       -- Pruefe Voraussetzung e(t)^2 > n
       | (et*et <= n) = error "e(t)^2 < n"
       -- Schritt 1 Pruefe ggt
       | (  ( (ggt (t*et) n) > 1) &&  ( (ggt (t*et) n) /= n ) ) = False 
       -- Schritt 3 (Loop on characters)
       -- | otherwise = error (ergstep3)
       | (ergstep3 == Nothing) = False 
       | ( not (jacobisumteststep5 restlpliste n t) ) = False  
		-- | otherwise = error "Test5 passed"
       | otherwise = jacobisumteststep6 n et t 1  
       where et = e t
             paare = -- loud "paare" $ 
                      getpairs t
             -- lplist-init aus Schritt 2
             lplist_init = -- loud "lplist_init" $ 
                            get_lplist_init n t
             ergstep3 = -- loud "ergstep3" $ 
                          jacobisumteststep3 s paare n t lplist_init
             restlpliste = -- loud "restlpliste" $ 
                            getlist ergstep3
             -- noch zu optimieren
\end{code}

-- Tester

\begin{code}
main = do
 args <- getArgs
 let s = read $ head args
     k = read (args!!1)
     n | length args < 4 =  2^k-1
       | read (args!!3) = 2^k-1
       | otherwise = k
     t | length args > 2 && read (args!!2) > 0 = read (args!!2)
       | otherwise = bestimme_t n
 putStrLn $ "Jacobi Sum Test.\nn = " ++ (show n) ++ ", t = " ++ (show t)
 putStrLn $ "Using skeleton: " ++
          case s of 
            0 -> "sequential legacy"
            1 -> "sequential map"
            2 -> "parallel map"
            3 -> "parallel farm"
            4 -> "parallel workpool, sorting, SHOULD BE blocking"
	    5 -> "parallel workpool, sorting, nonblocking (TAKE THIS)"
	    6 -> "parallel workpool, not sorting (DEPRICATED)"
 putStrLn "------------------------"
 print $ jacobisumtest s n t
 putStrLn "done"

jacobi s n = jacobisumtest s n (bestimme_t n)
\end{code}
