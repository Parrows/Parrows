Paralleles Jacobi Summentest

This is -*- Literate Haskell -*-

Autor: Oleg Lobachev
\begin{code}
{-# OPTIONS -cpp #-}
#ifdef INTERACTIVE
module ParJacobiSum where 
#else
module Main where
#endif

-- #define PARTIAL
import JacobiSum
import IterUntil

import Parallel.Eden (noPe, selfPe)
import System.IO.Unsafe (unsafePerformIO)
import System (getArgs)
import Data.Maybe
\end{code}

\begin{code}
-- Autor: Kent Kwee
import ModArithmetik
import Ringerweiterung 
import System (getArgs)
import Array
-- import List (elemIndex)
import Debug.Trace (trace)
loud s x = trace (s ++ " " ++ show x) x
\end{code}
\begin{seqcode}
-- | Liefert zu einem Element der Form Just [Integer] die Integerliste
getlist :: Maybe [Integer]  -- ^ Das Eingabeelement
      -> [Integer]    -- ^ Die Integerliste
getlist Nothing = error "Ergnistyp ist Nothing"
getlist (Just lpliste) = lpliste

[...]
-----------------------      
-- Hauptberechnungen --
-----------------------

-- Schritt 3 
jacobisumteststep3 :: [(Integer,Integer)] -- ^ (p,q) - Liste mit p^k || (q-1) | t
                 -> Integer             -- ^ n: zu pruefende Zahl
                 -> Integer             -- ^ t: Parameter t aus der Vorberechnung
                 -> [Integer]           -- ^ Liste von Primzahlen mit l_p = 0
                 -> Maybe [Integer]         -- ^ Das Ergebnis des Tests:
                                        --   Nothing: keine Primzahl
                                        --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep3 [] n t lpliste = -- loud "i haz win step 3" $ 
                                    Just lpliste 
jacobisumteststep3 ((p,q):rest) n t lpliste 
          | (erg == Nothing) =  Nothing
          | otherwise =  jacobisumteststep3 rest n t lpnew
          where k = nu (q-1) p  -- Vielfachheit von p in (q-1)
                erg = jacobisumteststep4 p k q n lpliste
                lpnew = getlist erg 
\end{nocode}

Skelett:
\begin{nocode}
iterUntil :: (Trans local, Trans task, 
              Trans subResult) => 
             localM ->
             [local] ->
             dataIn ->
             (dataIn -> [task]) ->
             (local -> task -> (subResult,local)) ->
             (localM -> [subResult] -> Either result ([task],localM)) ->
             result
\end{nocode}
\begin{code}
-- Author: Oleg Lobachev
jacobisumteststep3par :: [(Integer,Integer)] -- ^ (p,q) - Liste mit p^k || (q-1) | t
                      -> Integer             -- ^ n: zu pruefende Zahl
                      -> Integer             -- ^ t: Parameter t aus der Vorberechnung
                      -> [Integer]           -- ^ Liste von Primzahlen mit l_p = 0
                      -> Maybe [Integer]         -- ^ Das Ergebnis des Tests:
                                                 --   Nothing: keine Primzahl
                                                 --   Plist (Liste von Primzahlen mit l_p = 0)
{--
-- jacobisumteststep3 [] n t lpliste = Just lpliste 
type SingleCounter = Int
type Counter = (SingleCounter, SingleCounter)
type LocalM = DataIn
type Local = (Integer, Integer, Maybe [Integer])   -- -- ^ (k, l): task count out of total tasks
                                                   -- ^ n: zu pruefende Zahl und
                                                   -- ^ t: Parameter t aus der Vorberechnung
                                                   -- ^ Liste von Primzahlen mit l_p = 0
                                                   -- ^ die Liste wird stets erneuert!
type DataIn = (Counter, [Task])
type Task = (Integer, Integer) -- ^ (p,q) - Liste mit p^k || (q-1) | t
type SubResult = Maybe [Integer]
type Result = SubResult
splitTask :: DataIn -> [Task]
splitTask ((l, k), xs) = loud "Splitting tasks..." 
                       $ take (l-k) $ drop k xs -- ^ k tasks done, l tasks total
worker' :: Local -> Task -> (SubResult, Local)
-- worker' x@((k, l), _, _, _) _ | k = trace "Worker: counter overrun!" $ (Nothing, x)
worker' (n, t, _) _ = (Nothing, (n, t, Nothing)) -- wildcard
worker' x@(n, t, Nothing) _ = (Nothing, x)
worker' (n, t, Just lps) (p, q) = let res = loud "Step 4 has returned: " 
                                                    $ jacobisumteststep4 p v q n lps
                                      v = nu (q-1) p  -- Vielfachheit von p in (q-1)
                                  in ( res, (n, t, res) )
worker x y = -- trace ("Launching a worker at PE " ++ (show selfPe) ++ " with " ++ (show x) ++ " and " ++ (show y)) $
             worker x y
combineDecide' :: LocalM -> [SubResult] -> Either Result ([Task], LocalM)
combineDecide' ((l, k), pqs) lps | flats == Nothing = Left Nothing -- permature abort
                                 | k>=l = Left $ makeResult flats
                                 | otherwise = Right (splitTask ((l,k+nope),pqs), ((l,k+nope),pqs))
    where flats = flattenMaybe lps []
          makeResult (Just xs) = Just $ head xs -- remember, the list is reversed
          makeResult _ = Nothing                -- this should never happen

combineDecide x y = trace ("Combine\&Decide: " ++ (show x) ++ " and " ++ (show y))
                    $ combineDecide' x y

-- reverses the list!
flattenMaybe :: [Maybe a] -> [a] -> Maybe [a]
flattenMaybe [] ys = Just ys
flattenMaybe (x:xs) ys | isJust x
                           = flattenMaybe xs ((fromJust x):ys)
flattenMaybe _ _ = Nothing

foo (Just x) = [x]
foo Nothing = []

jacobisumteststep3par pqs n t lps = let input = ((l, 0), pqs)
                                        l = length pqs
                                        locals = loud "Cloning locals..." 
                                                 $ take l $ repeat (n, t, Just lps)
                                    in trace "Launching the skeleton..." 
                                           $ iterUntil input locals input splitTask worker combineDecide
--}
data Count = Int
data TotalCount = Int
data Counter = (Count, TotalCount)
data N = Integer
data T = Integer
data P = Integer
data Q = Integer
data PQ = Maybe (P, Q)
data L = Integer
data LPList = Maybe [L]
data Var = (Counter, PQ, LPList)
data Fix = (N, T)
data Local = Fix
data Datas = (Counter, [PQ], LPList)
data LocalM = Datas
data Task = Var
data SubR = Var
split :: Datas -> [Task]
split (c@(k,l), pqs, ls) = let pqlist = take (l-k) $ drop k pqs
                         in zip3 (repeat c) pqlist (repeat ls)
worker :: Local -> Task -> (SubR, Local)
worker x y@(_, _, Nothing) = (y, x)
worker x@(n, t) (c, Just (p, q), Just ls) = let res = loud "Step 4 has returned: " $
                                                      jacobisumteststep4 p v q n lps
                                                      v = nu (q-1) p  -- Vielfachheit von p in (q-1)
                                     in (c, Nothing, )

nope = max 1 noPe
\end{code}

weiter geht's...
\begin{junk}
-- original:
jacobisumteststep3 ((p,q):rest) n t lpliste 
          | (erg == Nothing) =  Nothing
          | otherwise =  jacobisumteststep3 rest n t lpnew
          where k = nu (q-1) p  -- Vielfachheit von p in (q-1)
                erg = jacobisumteststep4 p k q n lpliste
                lpnew = getlist erg 

-- | Schritt 4 vom Jacobisumtest
jacobisumteststep4 :: Integer   -- ^ Primzahl p
                 -> Integer   -- ^ Exponent k
                 -> Integer   -- ^ Primzahl q
                 -> Integer   -- ^ zu pruefende Zahl
                 -> [Integer] -- ^ Liste von Primzahlen mit l_p = 0
                 -> Maybe [Integer] -- ^ Das Ergebnis des Tests:
                                --   Nothing: keine Primzahl
                                --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep4 p k q spprime lpliste
                 | (p >= 3)  = jacobisumteststep4a p k q spprime lpliste fplist
                 | (k>=3)    = jacobisumteststep4b p k q spprime lpliste fplist
                 | (k==2)    = jacobisumteststep4c p k q spprime lpliste fplist
                 | (k==1)    = jacobisumteststep4d p k q spprime lpliste fplist
                 | otherwise = error "fail in selecting step4"
				  where fplist = fpairlist q  




-- | Schritt 4a vom Jacobisumtest
jacobisumteststep4a :: Integer   -- ^ Primzahl p
                  -> Integer   -- ^ Exponent k
                  -> Integer   -- ^ Primzahl q
                  -> Integer   -- ^ zu pruefende Zahl
                  -> [Integer] -- ^ Liste von Primzahlen mit l_p = 0
				   -> [(Integer,Integer)] -- ^ vorberchnetes f
                  -> Maybe [Integer] -- ^ Das Ergebnis des Tests:
                                --   Nothing: keine Primzahl
                                --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep4a p k q spprime lpliste fplist
              | (not $ isrootofunity spq p k spprime) = Nothing
              | (isprrootofunity spq p k spprime) = Just (remprime lpliste p)
              -- | (unitroottypeerg == Keine)     = Nothing
              -- | (unitroottypeerg == Primitive) = Just (remprime lpliste p)
              | otherwise                      = Just lpliste
                where emenge = [l | l <- [1..n], (l `mod` p) /= 0] 
                      theta = [(x ,multinverse x n) | x <- emenge ]  
                      alpha = [((r*x) `div` n ,multinverse x n) | x <- emenge ]  
                      r = spprime `mod` n
                      n = p^k
                      s1 = wgpot (j p q fplist) theta 
                      s2 = wpot s1 (spprime `div` n) 
                      spq = wmal s2 jpot
                      jpot = wgpot (j p q fplist) alpha
                      wpot b e = binpolypotsimmod b e p k spprime
                      wmal w1 w2 = malsimmod w1 w2 p k spprime
                      wgpot b e = gpotsimmod b e p k spprime
                      unitroottypeerg = unitroottype spq p k spprime


-- | Schritt 4b vom Jacobisumtest mit p=2, k>=3 
jacobisumteststep4b :: Integer   -- ^ Primzahl p
                  -> Integer   -- ^ Exponent k
                  -> Integer   -- ^ Primzahl q
                  -> Integer   -- ^ zu pruefende Zahl
                  -> [Integer] -- ^ Liste von Primzahlen mit l_p = 0
				   -> [(Integer,Integer)] -- ^ vorberchnetes f
                  -> Maybe [Integer] -- ^ Das Ergebnis des Tests:
                                --   Nothing: keine Primzahl
                                --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep4b p k q spprime lpliste fplist
              | (not $ isrootofunity s2q p k spprime) = Nothing
              | ((isprrootofunity s2q p k spprime) && (powermod q ((spprime -1) `div` 2) spprime  == (spprime - 1)))  = Just (remprime lpliste p)
              -- | (unitroottypeerg == Keine)  = Nothing
              -- | ((unitroottypeerg == Primitive)  && (powermod q ((spprime -1) `div` 2) spprime  == (spprime - 1)))  = Just (remprime lpliste p)			   
              | otherwise                         = Just lpliste
                where emenge =  [l | l <- [1..n], (l `mod` 8) == 1 || (l `mod` 8) == 3 ] 
                      theta =  [(x ,multinverse x n) | x <- emenge ]  
                      alpha =  [((r*x) `div` n ,multinverse x n) | x <- emenge ]  
                      r = spprime `mod` n
                      n =  2^k -- = p^k 
                      s1 = wgpot  (j3 q fplist) theta 
                      s2 = wpot s1 (spprime `div` n )
                      s2qtemp = wmal s2 jpot
                      s2q = if (delta==0) then s2qtemp else (wmal s2qtemp (j2 q fplist))
                      jpot = wgpot (j3 q fplist) alpha 
                      -- delta = if (elem r emenge) then 0 else 1
                      delta = if ( (r `mod` 8) == 1 || (r `mod` 8) == 3 ) then 0 else 1
                      wpot b e = binpolypotsimmod b e p k spprime
                      wmal w1 w2 = malsimmod w1 w2 p k spprime
                      wgpot b e = gpotsimmod b e p k spprime
                      -- unitroottypeerg = unitroottype s2q p k spprime

-- | Schritt 4c vom Jacobisumtest mit p=2, k=2 
jacobisumteststep4c :: Integer   -- ^ Primzahl p
                  -> Integer   -- ^ Exponent k
                  -> Integer   -- ^ Primzahl q
                  -> Integer   -- ^ zu pruefende Zahl
                  -> [Integer] -- ^ Liste von Primzahlen mit l_p = 0
				   -> [(Integer,Integer)] -- ^ vorberchnetes f
                  -> Maybe [Integer] -- ^ Das Ergebnis des Tests:
                                --   Nothing: keine Primzahl
                                --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep4c p k q spprime lpliste fplist
              | (not $ isrootofunity s2q p k spprime) = Nothing
              | ((isprrootofunity s2q p k spprime) && (powermod q ((spprime -1) `div` 2) spprime  == (spprime - 1)))  = Just (remprime lpliste p)
              -- | (unitroottypeerg == Keine)  = Nothing
              -- | ((unitroottypeerg == Primitive)  && (powermod q ((spprime -1) `div` 2) spprime  == (spprime - 1)))  = Just (remprime lpliste p)
              | otherwise                         = Just lpliste
                where s1 = polymodulo (skalmul q (wpot (j 2 q fplist)  2) ) spprime
                      s2 = wpot s1 (spprime `div` 4)
                      s2q = if (spprime `mod` 4 == 1) then s2 else (wmal s2 (wpot (j 2 q fplist) 2))
                      n = p^k
                      wpot b e = binpolypotsimmod b e p k spprime
                      wmal w1 w2 = malsimmod w1 w2 p k spprime
                      -- unitroottypeerg = unitroottype s2q p k spprime
                      -- es gibt nur diesen anderen Fall, da spprime ungerade

-- | Schritt 4d vom Jacobisumtest mit p=2, k=2 
jacobisumteststep4d :: Integer   -- ^ Primzahl p
                  -> Integer   -- ^ Exponent k
                  -> Integer   -- ^ kleine Primzahl q
                  -> Integer   -- ^ zu pruefende Zahl
                  -> [Integer] -- ^ Liste von Primzahlen mit l_p = 0
				   -> [(Integer,Integer)] -- ^ vorberchnetes f
                  -> Maybe [Integer] -- ^ Das Ergebnis des Tests:
                                --   Nothing: keine Primzahl
                                --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep4d p k q spprime lpliste fplist
              |  ( (not $ s2q == 1) && (not $ s2q == spprime -1)  ) = Nothing
              |  ((s2q == spprime -1) && (spprime `mod` 4 == 1)) = Just (remprime lpliste p)
              |  otherwise                         = Just lpliste
                 where s2q = powermod (spprime - q) ( (spprime-1) `div` 2) spprime
                 -- Fehlerhaftes Ergebnis fuer spprime = q, q aber klein spprime groß


-- | Schritt 5 des Jacobisumtests
jacobisumteststep5 :: [Integer]  -- ^ Liste von Primzahlen mit l_p = 0
                 -> Integer    -- ^ zu pruefende Zahl
                 -> Integer    -- ^ der Parameter t aus der Vorberechnung
                 -> Bool       -- ^ Gibt True als Ergebnis, falls Schritt 5 erfolgreich und n noch eine Primzahl sein koennte
-- jacobisumteststep5 [] _ _ = True
-- jacobisumteststep5 (p:ps) n t = (jacobisumteststep5single p q n t 30)  &&   (jacobisumteststep5 ps n t)
--              where q = p+1

jacobisumteststep5 ps n t = all (\p -> jacobisumteststep5single p (p+1) n t 30) ps

-- Hilfsfunktion fuer jacobisumtststep5
jacobisumteststep5single :: Integer    -- ^ Primzahl p
                       -> Integer    -- ^ moegliche Primzahl q = 1 (mod p)
                       -> Integer    -- ^ zu pruefende Zahl n
                       -> Integer    -- ^ der Parameter t aus der Vorberechnung
                       -> Integer    -- ^ Ein Zaehler fuer die Versuche
                       -> Bool       -- ^ Gibt True als Ergebnis, falls Schritt 5 erfolgreich und n noch eine Primzahl sein koennte
jacobisumteststep5single _ _ _ _ 0 = error "Test failed" 
jacobisumteststep5single p q n t counter 
             | ( (isPrime q) && (et `mod` q /= 0) && (ggt q n == 1) )  =  (jsum4erg /= Nothing) && ( (jsum4erg == Just []) || (jacobisumteststep5single p (q+p) n t (counter-1)))
             | otherwise = (jacobisumteststep5single p (q+p) n t (counter-1))
             where et = e t
                   k = nu (q-1) p
                   jsum4erg = jacobisumteststep4 p k q n [p]



-- start mit rinit=1 , n = spprime , count = t
-- | Schritt 6 vom Jacobisumtest
jacobisumteststep6 :: Integer -- ^ zu pruefende Zahl n
                 -> Integer -- ^ e(t)
                 -> Integer -- ^ Zaehlvariable fuer i
                 -> Integer -- ^ n^i 
                 -> Bool    -- ^ Ist n Primzahl? 
jacobisumteststep6 n et 0 rinit =  True
jacobisumteststep6 n et count rinit 
                         | ( (ri == 1) || (ri == n) || (n `mod` ri /= 0) ) = jacobisumteststep6 n et (count-1) ri
                         |otherwise = False
                         where ri = (rinit * n) `mod` et 
\end{junk}
\begin{code}
-- | Der Jacobisumtest                   
jacobisumtestpar :: Integer  -- ^ zu pruefende Zahl n
                 -> Integer  -- ^ der Parameter t aus der Vorberechnung
                 -> Bool     -- ^ Ist n Primzahl? 
jacobisumtestpar n t
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
             ergstep3 = loud "Parallel Step 3 launching..." $ 
                          jacobisumteststep3par paare n t lplist_init
             restlpliste = -- loud "restlpliste" $ 
                            getlist ergstep3
             -- noch zu optimieren
\end{code}
\begin{junk}
-------------------------------------------------------------
-- Rechnen mit PairPoly, um zu sehen, was effizienter ist ---
-------------------------------------------------------------

-- | Berechnung von J(p,q) fuer den Fall p>=3 oder (p=2,k=2)
pairpoly_j :: Integer  -- ^ Primzahl p
           -> Integer  -- ^ Primzahl q
           -> PairPoly     -- ^ Das Ergebnis J(p,q) aus Z[p^k-te EW], k Vielfachheit von p in q-1
pairpoly_j p q = normpairpoly [( (x + (f x q) ) `mod` n ,1) | x <- [1..(q-2)] ]
            where k=nu (q-1) p
                  n=p^k

-- | Berechnung von J_3(q) fuer den Fall p=2, k>=3
pairpoly_j3 :: Integer -- ^ Die Primzahl q  
            -> PairPoly    -- ^ Das Ergebnis J_3(q) aus Z[2^k-te EW], k Vielfachheit von 2 in q-1
pairpoly_j3 q = pairmult (pairpoly_j 2 q) (pairpoly_j3faktor q)


-- Hilfsfunktion zur Berechnung von j3(q)
pairpoly_j3faktor :: Integer  -> PairPoly
pairpoly_j3faktor q = normpairpoly [ ((2*x + (f x q) ) `mod` n,1) | x <- [1..(q-2)] ]
       where k=nu (q-1) 2
             n=2^k

-- | Berechnung von J_2(q) fuer den Fall p=2, k>=3
pairpoly_j2 :: Integer -- ^ Die Primzahl q  
 -> PairPoly    -- ^ Das Ergebnis J_2(q) aus Z[8-te EW], k Vielfachheit von 2 in q-1
pairpoly_j2 q = pairmult faktor faktor
   where faktor = pairpoly_j2faktor q

-- Hilfsfunktion zur Berechnung von J_2(q)        
pairpoly_j2faktor :: Integer  -> PairPoly
pairpoly_j2faktor q = normpairpoly [(3*x + (f x q) `mod` 8,1 ) | x <- [1..(q-2)] ]
       where k=nu (q-1) 2
             n=8
\end{junk}
\begin{code}
-- Tester

main = do
 args <- getArgs
 let n = read $ head args
     t = if (length args > 1)
         then read $ args!!1
         else bestimme_t n
 putStrLn $ "Jacobi Sum Test.\nn = " ++ (show n) ++ ", t = " ++ (show t)
 print $ jacobisumtestpar n t
 putStrLn "done"

test20par = all (\n -> jacobisumtestpar n 2) (primelist 132)
-- ergibt true

-- e(4)=240  --> n<= 240^2=57600
test80par = all (\n -> jacobisumtestpar n 4) (primelist 1000) 
-- ergibt true
testapar = all (\n -> jacobisumtestpar n 6) (primelist 200) 

-- jacobisumtest 79 2 = False

jacobipar n = jacobisumtestpar n (bestimme_t n)
\end{code}
\begin{junk}
-- Testfuntionen

-- n=2999 , t=6
-- getpairs 6 = [(2,3),(2,7),(3,7)]
-- lplist_init = [2]
-- test = jacobisumtest 2999 6

{-
eventuelle Optimierungsmöglichkeiten
- Teiler mit Primfaktorzerlegung 
  - ist so schon schnell genug
- Ineffiziente Vorberechnungen wegen unguenstiger Darstellung der EW
 --> Berechnungen mit PairPoly
    --> Vermeiden von plus [0,...,0,1,0,..0] [0,...,0,1,0,..0]
- Waehrend der Rechnung nur vereinfachen mit (n-te EW)^n=1 und (mod m) und erst am Ende mit Mipo
- Berechnung vom Typ der Einheitswurzel gleichzeitig
-}
\end{junk}