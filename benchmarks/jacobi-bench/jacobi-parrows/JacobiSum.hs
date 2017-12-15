{-# OPTIONS -cpp #-}
-- #ifdef INTERACTIVE
module JacobiSum where
-- #else 
-- module Main where
-- #endif
-- Autor: Kent Kwee
import ModArithmetik
import Ringerweiterung 
import System.Environment (getArgs)
import Data.Array

-- import List (elemIndex)
import Data.List (delete)
-- import Debug.Trace (trace)
-- loud s x = trace (s ++ " " ++ show x) x
trace _ x = x

-------------------------------------------
-- Hilfsfunktionen fuer Maybe [Integer]  --
-------------------------------------------

-- | Liefert zu einem Element der Form Just [Integer] die Integerliste
getlist :: Maybe [Integer]  -- ^ Das Eingabeelement
      -> [Integer]    -- ^ Die Integerliste
getlist Nothing = error "Ergnistyp ist Nothing"
getlist (Just lpliste) = lpliste

-- -- | Entfernt aus einer sortierten Liste ein Element 
-- remprime :: [Integer] -- ^ Eingabeliste
--        -> Integer   -- ^ zu entfernendes Element
--        -> [Integer] -- ^ Ausgabeliste
-- remprime [] p = []
-- remprime (l:ls) p 
--        | (p<l)  = l:(remprime ls p)
--        | (l==p) = ls 
--        | otherwise = (l:ls)         
remprime = flip delete

--------------------------------------------
-- Vorberechnungen fuer den Jacobisumtest --
--------------------------------------------

-- | Berechnung von e(t) 
e :: Integer    -- ^ der Parameter t
  -> Integer    -- ^ das Ergebnis  
e t = foldr (*) 2 [ (d+1)^( (nu t (d+1) ) +1)  |  d <- (teiler t), isPrime (d+1)  ]

-- | Bestimme ein t, so dass e(t)^2 > B
bestimme_t :: Integer  -- ^ obere Schranke B fuer die zu pruefenden Zahlen
         -> Integer  -- ^ Der Parameter t
--bestimme_t b = hilfsf 2 b
--            where hilfsf = (\x w -> if ((e x)*(e x) > w) then x else (hilfsf (x+1) w))
bestimme_t n 
        | ( n < 4292870400 ) = 12 
		 | ( n < 2^101  ) = 180
		 | ( n < 2^152  ) = 720
		 | ( n < 2^204  ) = 1260
		 | ( n < 2^268  ) = 2520
		 | ( n < 2^344  ) = 5040
		 | ( n < 2^525  ) = 27720
		 | ( n < 2^774  ) = 98280
		 | ( n < 2^1035 ) = 166320
		 | ( n < 2^1566 ) = 720720
		 | ( n < 2^2082 ) = 1663200
		 | ( n < 2^3217 ) = 8648640 
                 | ( n < 2^4253 ) = 691891200
                 | ( n < 2^44497) = 137944178989
		 | otherwise = hilfsf  137944178989 n 
            where hilfsf = (\x w -> if ((e x)*(e x) > w) then x else (hilfsf (x + x) w))
		 

-- | Berechung von f(x) von Vorberechnungsschritt 2.1
f_slow    :: Integer -- ^ x: Der Parameter x 
   -> Integer -- ^ q: Der Primzahlmodulus q fuer die primitive Wurzel g (mod q)
   -> Integer -- ^ Der diskrete Logarithmus f(x) mit 1 - g^x = g^(f(x)) 
f_slow x q = f2 1 (q + 1 -  (powermod pr x q)) pr q 
     where pr = primitiveRoot q
	  
-- Hilfsfunktion zur Bestimmung des primitiven Logarithmus        
f2 erg aim pr q
      | (aim == (powermod pr erg q)) = erg
      | otherwise = f2 (erg+1) aim pr q

f=f_slow

f_slowlist q = map (\x -> (x,f_slow x q)) [1..(q-2)]


-- unjust (Just a) = a
-- unjust (Nothing) = error "Not found"

pos :: Integer -> [Integer] -> Integer
pos _ [] = error "Not found"
pos e (l:ls)
       | (e==l)    = 1
		| otherwise = 1 + (pos e ls)

integertake :: Integer ->  [Integer] ->  [Integer]
integertake 0 _ = []
integertake n (l:ls) = l:(integertake (n-1) ls)
{-
prwlist :: Integer -> Integer -> [Integer]
prwlist pr q = pr:(map (\x -> x*pr `mod` q) $ prwlist pr q)
prwurzellist pr q = integertake (q-2) $ prwlist pr q
-}
{-
prwurzellist pr q = [powermod pr x q | x <- [1..(q-2)]]
aim pr q = map (\x -> q+1 -x) $ prwurzellist pr q
disklog pr q x=  (pos x (prwurzellist pr q) )
f_fasterlist pr q = map (disklog pr q)  (aim pr q)
-}
{-
fpairlist q = zip [1..(q-2)] (f_fasterlist) 
           where pr = primitiveRoot q
                 prwurzellist = [ powermod pr x q | x <- [1..(q-2)] ] --ohne powermod moeglich
                 aim = map (\x -> q+1 - x) $ prwurzellist
                 disklog  x =  (pos x (prwurzellist) )
                 f_fasterlist= map (disklog)  (aim)
-}

fpairlist q = zip [1..(q-2)] (f_fasterlist) 
           where pr = primitiveRoot q
                 prwurzellist = [ (powermod pr x q) | x <- [1..(q-2)] ]
                 prwlist = array (2,(q-1)) [ (powermod pr x q , x) | x <- [1..(q-2)] ]	
				  -- (zip [1,q-2]  prwurzellist) --			  
                 aim = map (\x -> q+1 - x) $ prwurzellist
                 disklog x = (prwlist!x)
                             -- (pos x (prwurzellist) )
                 f_fasterlist= map (disklog) (aim)
				  
                 -- prwlist = (array (1,(q-2) ) [ powermod pr x q | x <- [1..(q-2)] ]

-- Schritt 2.3 der Vorberechnung
-- Berechnung von J(p,q)

{-
-- | Berechnung von J(p,q) fuer den Fall p>=3 oder (p=2,k=2)
j :: Integer  -- ^ Primzahl p
-> Integer  -- ^ Primzahl q
-> Poly     -- ^ Das Ergebnis J(p,q) aus Z[p^k-te EW], k Vielfachheit von p in q-1
j p q = foldr (plus) [] [nrootpot (p^k) (x + (f x q) ) | x <- [1..(q-2)] ]
     where k=nu (q-1) p

-- | Berechnung von J_3(q) fuer den Fall p=2, k>=3
j3 :: Integer -- ^ Die Primzahl q  
 -> Poly    -- ^ Das Ergebnis J_3(q) aus Z[2^k-te EW], k Vielfachheit von 2 in q-1
j3 q = easysimp (mal (j 2 q) (j3faktor q)) (2^k)
    where k=nu (q-1) 2

-- Hilfsfunktion zur Berechnung von j3(q)
j3faktor :: Integer  -> Poly
j3faktor q = foldr plus [] [nrootpot (2^k) (2*x + (f x q) ) | x <- [1..(q-2)] ]
       where k=nu (q-1) 2


-- | Berechnung von J_2(q) fuer den Fall p=2, k>=3
j2 :: Integer -- ^ Die Primzahl q  
 -> Poly    -- ^ Das Ergebnis J_2(q) aus Z[8-te EW], k Vielfachheit von 2 in q-1
j2 q = easysimp (mal faktor faktor) (2^k) -- eigentlich 8
   where faktor = j2faktor q
         k = nu (q-1) 2

-- Hilfsfunktion zur Berechnung von J_2(q)        
j2faktor :: Integer  -> Poly
j2faktor q = foldr (plus) [] [nrootpot (2^k) (unterschied*(3*x + (f x q) ))  | x <- [1..(q-2)] ]
       where k=nu (q-1) 2
             unterschied = 2^(k-3)

-}

-- | Berechnung von J(p,q) fuer den Fall p>=3 oder (p=2,k=2)
j :: Integer  -- ^ Primzahl p
  -> Integer  -- ^ Primzahl q
  -> [(Integer,Integer)] -- ^ vorberchnetes f
  -> Poly     -- ^ Das Ergebnis J(p,q) aus Z[p^k-te EW], k Vielfachheit von p in q-1
j p q fplist = foldr (plus) [] [nrootpot (p^k) ( (\(x,y) -> x + y) f ) | f <- fplist ]
     where k=nu (q-1) p

-- | Berechnung von J_3(q) fuer den Fall p=2, k>=3
j3 :: Integer -- ^ Die Primzahl q  
   -> [(Integer,Integer)] -- ^ vorberchnetes f
   -> Poly    -- ^ Das Ergebnis J_3(q) aus Z[2^k-te EW], k Vielfachheit von 2 in q-1
j3 q fplist = easysimp (mal (j 2 q fplist) (j3faktor q fplist)) (2^k)
    where k=nu (q-1) 2

-- Hilfsfunktion zur Berechnung von j3(q)
j3faktor :: Integer   
        -> [(Integer,Integer)] -- ^ vorberchnetes f
        -> Poly
j3faktor q fplist = foldr plus [] [nrootpot (2^k) ( (\(x,y) -> 2*x + y) f  ) | f <- fplist ]
       where k=nu (q-1) 2


-- | Berechnung von J_2(q) fuer den Fall p=2, k>=3
j2 :: Integer -- ^ Die Primzahl q  
 -> [(Integer,Integer)] -- ^ vorberchnetes f
 -> Poly    -- ^ Das Ergebnis J_2(q) aus Z[8-te EW], k Vielfachheit von 2 in q-1
j2 q fplist = easysimp (mal faktor faktor) (2^k) -- eigentlich 8
   where faktor = j2faktor q fplist
         k = nu (q-1) 2

-- Hilfsfunktion zur Berechnung von J_2(q)        
j2faktor :: Integer  
        -> [(Integer,Integer)] -- ^ vorberchnetes f
        -> Poly
j2faktor q fplist = foldr (plus) [] [nrootpot (2^k) (unterschied*( (\(x,y) -> 3*x + y) f ) )  | f <- fplist ]
       where k=nu (q-1) 2
             unterschied = 2^(k-3)
			  

-- Hilfsfunktion zu lplist_init
get_lplist_init n t = [p| p <- primteiler t,  
                              not (p >= 3 && (not ((powermod n (p-1) (p*p)) == 1))) ]


-----------------------      
-- Hauptberechnungen --
-----------------------


-- Schritt 3 
-- | Liefert eine Liste von Primzahlpaaren (p,q) mit p^k || (q-1) | t
getpairs :: Integer              -- ^ Parameter t
       -> [(Integer,Integer)]  -- ^ (p,q) - Liste
getpairs t = getpairs2 qliste
         where qliste = [ q+1 | q <- (teiler t), isPrime (q+1) ]

-- Hilfsfunktion fuer getpairs
getpairs2 [] = []
getpairs2 (q:qs) = [ (p,q) | p <- (primteiler (q-1)) ]  ++ (getpairs2 qs)

jacobisumteststep3 :: [(Integer,Integer)] -- ^ (p,q) - Liste mit p^k || (q-1) | t
                 -> Integer             -- ^ n: zu pruefende Zahl
                 -> Integer             -- ^ t: Parameter t aus der Vorberechnung
                 -> [Integer]           -- ^ Liste von Primzahlen mit l_p = 0
                 -> Maybe [Integer]         -- ^ Das Ergebnis des Tests:
                                        --   Nothing: keine Primzahl
                                        --   Plist (Liste von Primzahlen mit l_p = 0)
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

-- | Schritt 4 vom Jacobisumtest
jacobisumteststep4 :: Integer   -- ^ Primzahl p
                 -> Integer   -- ^ Exponent k
                 -> Integer   -- ^ Primzahl q
                 -> Integer   -- ^ zu pruefende Zahl
                 -> [Integer] -- ^ Liste von Primzahlen mit l_p = 0
                 -> Maybe [Integer] -- ^ Das Ergebnis des Tests:
                                --   Nothing: keine Primzahl
                                --   Plist (Liste von Primzahlen mit l_p = 0)
jacobisumteststep4 p k q spprime lpliste = 
    trace ("In step 4: " ++ show (p, k, q, spprime, lpliste))
    jacobisumteststep4' p k q spprime lpliste

jacobisumteststep4' p k q spprime lpliste
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

jacobisumteststep5 ps n t = trace ("Step 5: to do " ++ show ps) $ 
                            all (\p -> jacobisumteststep5single p (p+1) n t 30) ps

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

-- | Der Jacobisumtest                   
jacobisumtest :: Integer  -- ^ zu pruefende Zahl n
            -> Integer  -- ^ der Parameter t aus der Vorberechnung
            -> Bool     -- ^ Ist n Primzahl? 
jacobisumtest n t
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
                          jacobisumteststep3 paare n t lplist_init
             restlpliste = -- loud "restlpliste" $ 
                            getlist ergstep3
             -- noch zu optimieren


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

-- Tester

-- main = do
--  -- args <- getArgs
--  -- let n = read $ head args
--  --     t = if (length args > 1)
--  --         then read $ args!!1
--  --         else bestimme_t n
--  args <- getArgs
--  let k = read $ head args
--      n | length args < 3 =  k
--        | read (args!!2) = 2^k-1
--        | otherwise = k
--      t | length args > 1 && read (args!!1) > 0 = read (args!!1)
--        | otherwise = bestimme_t n
--  putStrLn $ "Jacobi Sum Test.\nn = " ++ (show n) ++ ", t = " ++ (show t)
--  print $ jacobisumtest n t
--  putStrLn "done"
{-
 -- gotcha!
 $ ./jacobi 31 
 Jacobi Sum Test.
 n = 31, t = 2
 False
 done

 31 ist aber prim!
-}


test20 = all (\n -> jacobisumtest n 2) (primelist 132)
-- ergibt true

-- e(4)=240  --> n<= 240^2=57600
test80 = all (\n -> jacobisumtest n 4) (primelist 1000) 
-- ergibt true
testa = all (\n -> jacobisumtest n 6) (primelist 200) 

-- jacobisumtest 79 2 = False

jacobi n = jacobisumtest n (bestimme_t n)


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

