module ModArithmetik where 
-- Autor: Kent Kwee
import Primes

-- | Modulares Potenzieren: b^e mod m
powermod             :: Integer    -- ^ b ist die Basis
                   -> Integer    -- ^ e ist der positive Exponent
                   -> Integer    -- ^ m der Modul
                   -> Integer    -- ^ Das Ergebnis b^e mod m
powermod b e m 
 | (e==0)           = 1
 | (e `mod` 2 == 0) = (temp * temp) `mod` m 
 | otherwise        = b * (powermod b (e-1) m) `mod` m 
 where temp = (powermod b (e `div` 2) m) 

-- | ggT von m und n
ggt :: Integer -- ^ m: erste Zahl 
  -> Integer -- ^ n: zweite Zahl
  -> Integer -- ^ der ggt (m,n)
ggt m n
 | (n==0)    = m
 | otherwise = ggt n (m `mod` n)

-- | erweiterter Euklid
-- exteuklid a b liefert (d,s,t) mit (a,b) = d = a*s+b*t 
exteuklid       :: Integer    -- ^ Zahl a
              -> Integer    -- ^ Zahl b
              -> (Integer,Integer,Integer) -- (d,s,t)
exteuklid a 0 = (a,1,0)
exteuklid a b = (d,t,s - (a `div` b) *t)
            where (d,s,t)= exteuklid b (a `mod` b)

-- | Liefert zu einer Restklasse die kleinsten nicht-negativen Repräsentanten
normalize    :: Integer -- ^ Repraesentant a
           -> Integer -- ^ modulo p
           -> Integer -- ^ kleinster nicht-negativer Repräsentant
normalize a p | a>=0 && a<p = a
            | a>=0 && a>p = a `mod` p
            | a<0         = (p + (a `mod` p)) `mod` p

-- | findet das multiplikative Inverse zu a (mod n)
multinverse :: Integer -- ^ Repräsentant a
          -> Integer -- ^ Modulus n
          -> Integer -- ^ Ergebnis e mit a*e= 1 (mod n) 
multinverse a n 
          | (d==1)    = normalize s n
          | otherwise = error ("Es existiert kein multikativ Inverses von " ++ (show a) ++ " zur Basis " ++ (show n) ++ ".")
          where (d,s,t) = exteuklid a n

-- | Liefert die eine Liste von Teilern von n
teiler :: Integer   -- ^ n: Die Zahl, von der die Teier bestimmt werden sollen
     -> [Integer] -- ^ Die Liste der Teiler von n
teiler n = [a | a <- [1..n], n `mod` a == 0]   

primelist  :: Integer   -- ^ n: Die obere Schranke für die Primzahlen
         -> [Integer] -- ^ Die Liste der Primzahlen unter n
primelist n = takeWhile (<= n) primes

-- | Liefert die eine Liste der Primteiler von n
primteiler :: Integer   -- ^ n: Die Zahl, von der die Primteiler bestimmt werden sollen
         -> [Integer] -- ^ Die Liste der Teiler von n
primteiler n = [p | p <- (primelist n), n `mod` p == 0]


-- | Liefert eine unendliche Primzahlliste
--primes :: [Integer] -- ^ Unendliche Primzahlliste
--primes = 2 : primes' 3 [5,7..] 
--       where primes' curPrime rest = curPrime : primes' nextPrime nextRest -- '
--                                   where (nextPrime:nextRest) = filter (\x -> mod x curPrime /= 0) rest

-- | Prueft ob eine Zahl eine Primzahl ist                                                
--isPrime :: Integer -- ^ zu pruefende Zahl
--        -> Bool    -- ^ Das Ergebnis
--isPrime = sieve1 primes
--        where sieve1 :: [Integer] -> Integer -> Bool
--              sieve1 pl x = and $ map (\y -> x `mod` y /= 0) (takeWhile (\a -> a*a <= x) pl)

-- | Prueft ob eine Zahl eine Primzahl ist       
-- Methode uebernommen von Numbers-0.2.1
isPrime :: Integer -> Bool
isPrime n      = all (not .(\p-> (n `mod` p) == 0)) $ takeWhile (\p -> p*p <= n) primes


-- | Berechnung einer primitiven Wurzel g_p modulo p fuer p >= 3
primitiveRoot :: Integer -- ^ p: der ungerade Primzahl-Modulus
            -> Integer -- ^ die primitive Wurzel (mod p)
primitiveRoot p = primitiveRoot2 p 2 ls ls
              where ls = primteiler (p-1)

-- Hilfsfunktion
-- Aufruf mit primitiveRoot2 p 2 (primteiler (p-1)) (primteiler (p-1))
primitiveRoot2 p a [] rs = a 
primitiveRoot2 p a (q:qs) rs
 | ( (powermod a ((p-1) `div` q ) p) == 1) = primitiveRoot2 p (a+1) rs rs
 | otherwise                               = primitiveRoot2 p a qs rs


-- | Berechnung der Vielfachheit von q in t
nu ::     Integer     -- ^ t: zu pruefende Zahl
        -> Integer  -- ^ q: Zahl deren Vielfachheit geprueft wird
        -> Integer  -- ^ Vielfachheit von q in t
nu t q
 | (t `mod` q == 0) = 1 + nu (t `div` q) q
 | otherwise        = 0 

