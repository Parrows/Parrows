module Ringerweiterung where 
-- Autor: Kent Kwee

-- Realisierung des Polynomrings als Integerliste
-- a + bx +cx^2 + .... < -- > [a,b,c,...] 
type Poly = [Integer]

-- Hilfsfunktionen fuer Listen

-- | splittet eine Liste [a] in Listen der Laenge n
splitlist :: [a]      -- ^ l: zu splittende Liste
        -> Integer  -- ^ n: Laenge der Teillisten
        -> [[a]]    -- ^ Liste der Listen der Laenge n
splitlist [] _ = []
splitlist l n = anfang:(splitlist rest n) 
           where (anfang,rest) = zerlegebei l n   

-- | splitat fuer Integer
zerlegebei :: [a]       -- ^ l: zu splittende Liste
         -> Integer   -- ^ n: Position der Zerlegung
         -> ([a],[a]) -- ^ Anfang und Rest als Paar
zerlegebei as 0 = ([],as)
zerlegebei [] _ = ([],[])
zerlegebei (a:as) n = (a:anfang,rest)
                   where (anfang,rest) = zerlegebei as (n-1)

-- | Addiert zwei Polynome
plus :: Poly -- ^ erster Summand
   -> Poly -- ^ zweiter Summand
   -> Poly -- ^ Summe
plus [] b = b
plus a [] = a
plus (a:as) (b:bs) = (a+b):(plus as bs)


-- | Multipliziert zwei Polynome
mal  :: Poly -- ^ erster Faktor
   -> Poly -- ^ zweiter Faktor
   -> Poly -- ^ Produkt
mal [] b = []
mal a [] = []
mal (a:as) (b:bs) = (a*b):(plus (plus (mal [a] bs) (mal [b] as)) (0:(mal as bs)))

-- | Bildet die n-te Potenz eine Polynoms
polypot :: Poly     -- ^ Basis
      -> Integer  -- ^ Exponent
      -> Poly     -- ^ Potenz
polypot polynom 0 = [1]
polypot polynom 1 = polynom
polypot polynom n = mal polynom (polypot polynom (n-1))  

-- | Bildet die n-te Potenz eine Polynoms, das ein Element in Z[p^k-te Einheitswurzel] repraesentiert
polypotsim :: Poly     -- ^ Basis
         -> Integer  -- ^ Exponent
         -> Integer  -- ^ Primzahl p
         -> Integer  -- ^ Exponent k zur Primzahl p
         -> Poly     -- ^ Ergebnis
polypotsim polynom 0 _ _ = [1]
polypotsim polynom 1 p k = simpol polynom p k
polypotsim polynom n p k = simpol (mal polynom (simpol (polypotsim polynom (n-1) p  k) p k )) p k   

-- | Bildet die n-te Potenz eine Polynoms, das ein Element in Z[p^k-te Einheitswurzel] repraesentiert.
-- Die Funktion verwendet dabei den binaeren Potenzierungsalgorithmus
binpolypotsim :: Poly     -- ^ Basis
            -> Integer  -- ^ positiver Exponent
            -> Integer  -- ^ Primzahl p
            -> Integer  -- ^ Exponent k zur Primzahl p
            -> Poly     -- ^ Ergebnis
binpolypotsim _ 0 _ _ = [1]
binpolypotsim b n p k 
                 | (even n)  = simpol (mal temp temp) p k 
                 | otherwise = simpol (mal b (binpolypotsim b (n-1) p k)) p k
                 where temp = (binpolypotsim b (n `div` 2) p k)  


-- | Bildet die n-te Potenz eine Polynoms, das ein Element in Z_m[p^k-te Einheitswurzel] repraesentiert.
-- Die Funktion verwendet dabei den binaeren Potenzierungsalgorithmus
binpolypotsimmod :: Poly     -- ^ Basis
                -> Integer  -- ^ positiver Exponent
                -> Integer  -- ^ Primzahl p
                -> Integer  -- ^ Exponent k zur Primzahl p
                -> Integer  -- ^ Der Modulus m                 
                -> Poly     -- ^ Ergebnis
binpolypotsimmod _ 0 _ _ _ = [1]
binpolypotsimmod b n p k m
                 | (even n)  = malsimmod temp temp p k m
                 | otherwise = malsimmod b (binpolypotsimmod b (n-1) p k m) p k m
                 where temp = (binpolypotsimmod b (n `div` 2) p k m)  

-- | Bildet das Produkt zweier Polynome, die Elemente in Z_m[p^k-te Einheitswurzel] repraesentieren.
malsimmod :: Poly     -- ^ 1. Faktor
         -> Poly     -- ^ 2. Faktor
         -> Integer  -- ^ Primzahl p
         -> Integer  -- ^ Exponent k zur Primzahl p
         -> Integer  -- ^ Der Modulus m                 
         -> Poly     -- ^ Ergebnis
malsimmod w1 w2 p k m = polymodulo ( simpol (mal w1 w2) p k ) m

-- | Multipliziert ein Polynom mit einer ganzen Zahl
skalmul :: Integer  -- ^ Faktor k
      -> Poly     -- ^ Polynom
      -> Poly     -- ^ k*Polynom
skalmul _ [] = []
skalmul k (a:as) = (k*a):(skalmul k as)

-- | Prueft, ob ein Polynom das Nullpolynom ist
iszeropoly :: Poly  -- ^ zu pruefendes Polynom
         -> Bool  -- ^ Ergebnis
iszeropoly []     = True
iszeropoly (a:as) = (a==0) && (iszeropoly as)


-- | Prueft, ob ein Polynom das Einspolynom ist ist
isunitpoly :: Poly  -- ^ zu pruefendes Polynom
         -> Bool  -- ^ Ergebnis
isunitpoly []     = False
isunitpoly (l:ls) = (l==1) && iszeropoly ls


-- | Reduziert ein Polynom modulo m
polymodulo :: Poly    -- ^ zu reduzierendes Polynom
         -> Integer -- ^ Modulus m
         -> Poly    -- ^ Ergebnis
polymodulo [] _ = []
polymodulo (l:ls) m = (l `mod` m):(polymodulo ls m)

-- | Liefert (n-te Einheitswurzel)^e in Z[n-te Einheitswurzel] mit Ausnutzen von (n-te Einheitswurzel)^n=1
nrootpot :: Integer   -- ^ Zahl p
       -> Integer   -- ^ Exponent der Einheitswurzel
       -> Poly      -- ^ Ergebnis
nrootpot n e = nrootpot2 n (e `mod` n)

-- | Liefert (n-te Einheitswurzel)^e in Z[n-te Einheitswurzel]
nrootpot2 :: Integer   -- ^ Zahl p
        -> Integer   -- ^ Exponent der Einheitswurzel
        -> Poly      -- ^ Ergebnis
nrootpot2 n 0 = [1]
nrootpot2 n e  = 0:(nrootpot2 n (e-1))


-- | Liefert k*(n-te Einheitswurzel)^e in Z[n-te Einheitswurzel]
nrootpot3 :: Integer   -- ^ Skalar k
        -> Integer   -- ^ Zahl n
        -> Integer   -- ^ Exponent der Einheitswurzel
        -> Poly      -- ^ Ergebnis
nrootpot3 k n 0 = [k]
nrootpot3 k n e  = 0:(nrootpot3 k n (e-1))

-- | Liefert das Minimalpolynom fuer die p^k-te Einheitswurzel 
mipo :: Integer -- ^ Primzahl p
   -> Integer -- ^ Exponent k
   -> Poly    -- ^ Minimalpolynom von p^k-te Einheitswurzel
mipo p k = foldr (plus) [] [ nrootpot (p^k) (i*p^(k-1)) | i <- [0..(p-1)]]

-- | Liefert das s*Minimalpolynom fuer die p^k-te Einheitswurzel      
mipomul :: Integer -- ^ Primzahl p
      -> Integer -- ^ Exponent k
      -> Integer -- ^ Skalar s
      -> Poly    -- ^ s * Minimalpolynom von p^k-te Einheitswurzel
mipomul p k s = foldr (plus) [] [ nrootpot3 s (p^k) (i*p^(k-1)) | i <- [0..(p-1)]]

-- | Liefert das Minimalpolynom fuer die p^k-te Einheitswurzel - (n-te EW)^( (p-1) * p^(k-1) )
mipoohneleztes :: Integer -- ^ Primzahl p
             -> Integer -- ^ Exponent k
             -> Poly    -- ^ Minimalpolynom von p^k-te Einheitswurzel - (n-te EW)^( (p-1) * p^(k-1) )
mipoohneleztes p k = foldr (plus) [] [ nrootpot (p^k) (i*p^(k-1)) | i <- [0..(p-2)]]


-- | Vereinfacht ein Polynom das ein Element in der Ringerweiterung Z[n-te Einheitswurzel]
--   repraesentiert durch Ausnutzen (n-te Einheitswurzel)^n=1                     
easysimp :: Poly     -- ^ Das zu vereinfachende Polynom
       -> Integer  -- ^ n von der Einheitswurzel
       -> Poly     -- ^ Das vereinfachte Polynom
easysimp l n = foldr (plus) [] (splitlist l n) 

-- | Vereinfacht ein Polynom das ein Element in der Ringerweiterung Z[p^k-te Einheitswurzel]
--   repraesentiert durch Ausnutzen (n-te Einheitswurzel)^n=1 und Verwendung des Minimalpolynoms.
simpol :: Poly    -- ^ zu vereinfachendes Polynom
     -> Integer -- ^ Primzahl p
     -> Integer -- ^ Exponent k von p
     -> Poly    -- ^ vereinfachtes Polynom
simpol pol p k = plus anfang (mal  (skalmul (-1) rest) (mipoohneleztes p k))
              where (anfang, rest) = zerlegebei (easysimp pol (p^k)) ((p-1)*p^(k-1))


-- | Verwandelt ein Polynom in eine ganze Zahl, falls es eine ganze Zahl ist.
polytointeger :: Poly    -- ^ zu verwandelndes Polynom
            -> Integer -- ^ die ganze Zahl
polytointeger [] = 0
polytointeger (a:as)
       | (iszeropoly as) = a
       | otherwise   = error "Dies ist keine ganze Zahl"

-- | Laesst sigma_a auf ein ein Element aus Z[n-te EW] operieren
sigma :: Integer  -- ^ a (Potenz zu der die EW erhoben werden) also sigma_a
    -> Integer  -- ^ n  (das n von der Einheitswurzel)
    -> Poly     -- ^ Eingabelement
    -> Poly     -- ^ Ausgabeelement
sigma a n ps  = sigma2 a n ps 0

-- Hilfsfunktion fuer sigma
sigma2 :: Integer -> Integer -> Poly -> Integer -> Poly
sigma2 _ _ [] _ = []
sigma2 a n (p:ps) 0 = plus [p] (sigma2 a n ps 1)  -- Z bleibt fix
sigma2 a n (p:ps) k = plus (nrootpot3 p n ( (k*a) `mod` n)) (sigma2 a n ps (k+1))


-- | berechne Potenz Z(n-te EW) ^ Z(G) 
gpot :: Poly                -- ^ Die Basis aus Z[n-te EW]
   -> [(Integer,Integer)] -- ^ Die Funktion f aus Z(G)
   -> Integer             -- ^ n von der EW 
   -> Poly                -- ^ Das Ergebnis aus Z[n-te EW]
gpot b [] n = [1]
gpot b ((f_sigma,sigma_a):es) n = mal (polypot (sigma sigma_a n b) f_sigma) (gpot b es n) 


-- | berechne Potenz Z(p^k-te EW) ^ Z(G) mod m
gpotsimmod :: Poly                -- ^ Die Basis aus Z[n-te EW]
          -> [(Integer,Integer)] -- ^ Die Funktion f aus Z(G) mit f entpricht [(f_sigma,sigma)]
          -> Integer             -- ^ Primzahl p
          -> Integer             -- ^ Exponent k zur Primzahl p
          -> Integer             -- ^ Der Modulus m    
          -> Poly                -- ^ Das Ergebnis aus Z[n-te EW]
gpotsimmod b [] p k m         = [1]
gpotsimmod b ((f_sigma,sigma_a):es) p k m = wmal (wpot (sigma sigma_a n b) f_sigma) (gpotsimmod b es p k m) 
                             where wpot b y = binpolypotsimmod b y p k m
                                   wmal w1 w2 = malsimmod w1 w2 p k m
                                   n = p^k

-- | Prueft ob ein Element aus Z_m[p^k-te EW] , repraesentiert durch ein Polynom, eine Einheitswurzel ist
isrootofunity :: Poly      -- ^ Das zu pruefende Element
             -> Integer  -- ^ Primzahl p
             -> Integer  -- ^ Exponent k
             -> Integer  -- ^ Modulus m
             -> Bool     -- ^ Das Ergebnis
isrootofunity l p k m = isunitpoly erg
                   where temp = simpol l p k
                         temp2 = polymodulo temp m
                         erg = wpot temp2 n
                         n = p^k
                         wpot b n = binpolypotsimmod b n p k m

-- | Prueft ob ein Element aus Z_m[p^k-te EW] , repraesentiert durch ein Polynom, eine primitive Einheitswurzel ist.
isprrootofunity :: Poly      -- ^ Das zu pruefende Element
             -> Integer  -- ^ Primzahl p
             -> Integer  -- ^ Exponent k
             -> Integer  -- ^ Modulus m
             -> Bool     -- ^ Das Ergebnis
isprrootofunity l p k m = -- (isrootofunity l p k m) && vorher wird isrootofunity geprueft
                         (not $ isunitpoly erg)
                   where temp = simpol l p k
                         temp2 = polymodulo temp m
                         erg = wpot temp2 n
                         wpot b n = binpolypotsimmod b n p k m
                         n = p^(k-1)

data UnitRootType = Keine | Einfache | Primitive
                   deriving (Show,Eq)

-- | Bestimmt den Typ der primitiven Wurzel
unitroottype :: Poly            -- ^ Das zu pruefende Element l
             -> Integer        -- ^ Primzahl p
             -> Integer        -- ^ Exponent k
             -> Integer        -- ^ Modulus m
             -> UnitRootType  -- ^ Das Ergebnis
unitroottype l p k m 
      | (not $ isunitpoly erg1) = Keine
	   | (isunitpoly erg2)       = Primitive
	   | otherwise	             = Einfache
	   where erg1 = wpot erg2 p 
	         erg2 = wpot l ( p^(k-1) )
	         wpot b e = binpolypotsimmod b e p k m
			 
-- erg1 = wpot erg2 p   --  l^(p^(k-1))^p  = l^( (p^(k-1) *p)) =  l^(p^k)

			 
-- Repräsentiert ein Polynom als Liste von Paaren (e,a) entspricht a*x^e
type PairPoly = [(Integer,Integer)] 

-- | Verwandelt Poly in PairPoly
polytopairpoly :: Poly -- ^ Eingabepolynom
             -> PairPoly -- ^ AusgabePoly
polytopairpoly p = normpairpoly $ polytopairpoly2 p 0 where
   polytopairpoly2 [] _ = []
   polytopairpoly2 (p:ps) k = (k,p):(polytopairpoly2 ps (k+1))

-- | Verwandelt PairPoly in Poly
pairpolytopoly :: PairPoly -- ^ EingabePoly
             -> Poly     -- ^ AusgabePoly
pairpolytopoly pp = pairpolytopoly2 (normpairpoly pp) 0 where
  pairpolytopoly2 [] _ = []
  pairpolytopoly2 ((e,a):rest) k 
       | (e == k)  = a:(pairpolytopoly2 rest (k+1))
       | (k < e )  = 0:(pairpolytopoly2 rest (k+1))
       | otherwise = error "Fehler"

-- | Zeigt PairPoly an
showpairpoly :: PairPoly -> String 
showpairpoly [] = "0"
showpairpoly [(e,a)] = (show a) ++ "*x^" ++ (show e)
showpairpoly ((e,a):rest) =  (show a) ++ "*x^" ++ (show e) ++ " + " ++ (showpairpoly rest)

-- | Addiert zwei Polynome unnormierte Polynome
unsortedpairadd :: PairPoly -- ^ 1. Summand
              -> PairPoly -- ^ 2. Summand
              -> PairPoly -- ^ Summe (nicht normiert und vereinfacht)
unsortedpairadd a b = a ++ b

-- | Addiert zwei normierte und vereinfachte Polynome
pairadd :: PairPoly -- ^ 1. Summand
      -> PairPoly -- ^ 2. Summand
      -> PairPoly -- ^ Summe (normiert)
pairadd a [] = a
pairadd [] b = b
pairadd ((a1,a2):as) ((b1,b2):bs) 
            | (a1 <  b1) = (a1,a2):(pairadd as ((b1,b2):bs))
            | (a1 >  b1)  = (b1,b2):(pairadd ((a1,a2):as) bs)              
            | (a1 == b1) && (a2+b2 /= 0) = (a1,a2+b2):(pairadd as bs)
            | otherwise = pairadd as bs         


-- split von http://en.literateprograms.org/Merge_sort_(Haskell)
-- split :: [a] -> ([a],[a])
-- split xs = splitrec xs xs []

splitrec :: [a] -> [a] -> [a] -> ([a],[a])
splitrec [] ys zs             = (reverse zs, ys)
splitrec [x] ys zs            = (reverse zs, ys)
splitrec (x1:x2:xs) (y:ys) zs = splitrec xs ys (y:zs) 

split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:xs,y:ys)
    where 
      (xs,ys) = split zs


-- | Normiert ein Polynom durch Sortieren
normpairpoly :: PairPoly  -- ^ zu normierendes Polynom
           -> PairPoly  -- ^ das normierte Polynom
normpairpoly [] = []
normpairpoly [(e,0)] = []
normpairpoly [(e,a)] = [(e,a)]
normpairpoly xs = pairadd (normpairpoly anfang) (normpairpoly ende)
              where (anfang,ende) = split xs



-- | Multipliziert zwei Polynome
pairmult :: PairPoly  -- ^ 1. Faktor
       -> PairPoly  -- ^ 2. Faktor    
       -> PairPoly  -- ^ Produkt
pairmult _ [] = []
pairmult [] _ = []
pairmult ((a1,a2):as) ((b1,b2):bs)  = pairadd [(a1+b1,a2*b2)] (pairadd  (pairadd (pairmult [(a1,a2)] bs) (pairmult as bs)) (pairmult as  [(b1,b2)])) 

-- | Potenziert ein Polynom
pairpot :: PairPoly  -- ^ Basis
      -> Integer   -- ^ Exponent
      -> PairPoly  -- ^ Ergebnis
pairpot _ 0 = [(0,1)]
pairpot b 1 = b
pairpot b e 
     | (even e) = pairmult temp temp
     | otherwise = pairmult b (pairpot b (e-1))
     where temp = pairpot b (e `div` 2)              

-- | Potenziert ein Polynom mit Vereinfachung (n-te EW)^n = 1
pairpoteasysimp :: PairPoly  -- ^ Basis
      -> Integer   -- ^ Exponent
      -> Integer   -- ^ n zur n-ten Einheitswurzel
      -> PairPoly  -- ^ Ergebnis
pairpoteasysimp _ 0 _ = [(0,1)]
pairpoteasysimp b 1 n = paireasysimp b n
pairpoteasysimp b e n
     | (even e) = paireasysimp (pairmult temp temp) n 
     | otherwise = paireasysimp (pairmult b (pairpoteasysimp b (e-1) n)) n 
     where temp =  pairpoteasysimp b (e `div` 2)  n       


-- | Potenziert ein Polynom
pairpotnorm :: PairPoly  -- ^ Basis
          -> Integer   -- ^ Exponent
          -> PairPoly  -- ^ Ergebnis
pairpotnorm b e = normpairpoly $  pairpot b e 


-- | Vereinfacht ein Polynom das ein Element in der Ringerweiterung Z[n-te Einheitswurzel]
--   repraesentiert durch Ausnutzen (n-te Einheitswurzel)^n=1                     
paireasysimp :: PairPoly     -- ^ Das zu vereinfachende Polynom
          -> Integer  -- ^ n von der Einheitswurzel
          -> PairPoly     -- ^ Das vereinfachte Polynom
paireasysimp pp n = normpairpoly $ map (\(a,b) -> ((a `mod` n) , b)) pp


-- | Reduziert ein Polynom modulo m                   
pairmodulo :: PairPoly     -- ^ Das zu vereinfachende Polynom
          -> Integer  -- ^ n der Modulus
          -> PairPoly     -- ^ Das vereinfachte Polynom
pairmodulo pp m = normpairpoly $ map (\(a,b) -> (a , (b `mod`m ))) pp

-- | Laesst sigma_a auf ein ein Element aus Z[n-te EW] operieren
pairsigma :: Integer   -- ^ a (Potenz zu der die EW erhoben werden)
        -> Integer   -- ^ n 
        -> PairPoly  -- ^ Eingabelement
        -> PairPoly  -- ^ Ausgabeelement
pairsigma a n pp = normpairpoly $ map (\(e,s) -> (((e*a) `mod` n) , s)) pp


-- | Vereinfacht ein Polynom mit Minimalpolynom     
simpairpoly :: PairPoly    -- ^ zu vereinfachendes Polynom
          -> Integer     -- ^ Primzahl p
          -> Integer     -- ^ Exponent k von p
          -> PairPoly    -- ^ vereinfachtes Polynom
simpairpoly pp p k = normpairpoly $ normpairpoly $ simpairpoly2 pp p k where            
     simpairpoly2  [] _ _ = []
     simpairpoly2 ((e,a):rest) p k  
              | ( diff < 0 ) = (red,a):(simpairpoly2 rest p k) 
              | otherwise = pairadd (simpairpoly2 rest p k) [ (diff +  i * (p^(k-1)) , -a) | i <- [0..(p-2)]]
               where n=p^k 
                     red= e `mod` n
                     diff = red - (p-1) * p^(k-1)

-- | berechne Potenz Z(n-te EW) ^ Z(G) in 
gpotpairpoly :: PairPoly                -- ^ Die Basis in Z[n-te EW]
           -> [(Integer,Integer)] -- ^ Die Funktion f aus Z(G)
           -> Integer             -- ^ n von der EW 
           -> PairPoly                -- ^ Das Ergebnis in Z[n-te EW]
gpotpairpoly b [] n = [(0,1)]
gpotpairpoly b ((a,e):es) n = paireasysimp (pairmult (pairpotnorm (pairsigma a n b) e) (gpotpairpoly b es n)) n



test12 = simpairpoly [(1,1),(2,1)] 3 1
-- kommt das gleich raus?
test31 = simpol [1,2,3,4,5,6,7,8,9] 3 2
test32 =  pairpolytopoly $ simpairpoly (polytopairpoly [1,2,3,4,5,6,7,8,9]) 3 2

