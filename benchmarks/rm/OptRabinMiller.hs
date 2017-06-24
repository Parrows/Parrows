module OptRabinMiller where 
-- Autor: Kent Kwee
-- Optimised by Oleg Lobachev, 2011

import System.Random

import System.IO.Unsafe (unsafePerformIO) -- HAAACK!

-- Hilfsfunktionen
	   
-- | Zerlege n in gerade und ungerade Anteile
zerlege               ::(Integer,Integer)   -- ^ Eingabe (n,0)
                     -> (Integer,Integer)  -- ^ Ausgabe (q,t) mit n=2^t*q
{-
zerlege (q,t) 
  | (q `mod` 2 == 0) = zerlege(q `div` 2,t+1)
  | otherwise        = (q,t)
-}

zerlege (q,t) 
  | (r == 0) = zerlege (k, t+1)
  | otherwise        = (q, t)
  where (k, r) = divMod q 2

zerlege1 (q,t) 
  | (r == 0) = zerlege1(k, t+1)
  | otherwise        = (q, t)
  where (k, r) = divMod q 2


-- | Modulares Potenzieren: b^e mod m
-- | TODO: A faster powering algorithm
powermod             :: Integer    -- ^ b ist die Basis
                    -> Integer    -- ^ e ist der Exponent
                    -> Integer    -- ^ m der Modul
                    -> Integer    -- ^ Das Ergebnis b^e mod m
{-
powermod b e m 
  | (e==0)           = 1
  | (e `mod` 2 == 0) = (temp * temp) `mod` m 
  | otherwise        = b * (powermod b (e-1) m) `mod` m 
  where temp = (powermod b (e `div` 2) m) 
-}
powermod b e m 
  | e==0      = 1
  | r==0      = (binrec * binrec) `mod` m 
  | otherwise = b * (powermod b (e-1) m) `mod` m 
  where binrec = powermod b d m
        (d, r) = divMod e 2

{-
-- bloedsinn?
powermod b e m 
  | e==0      = 1
  | r==0      = (binrec * binrec) `mod` m 
  | p==0      = (trerec * trerec * trerec) `mod` m
  | otherwise = b * (powermod b (e-1) m) `mod` m 
  where binrec = powermod b d m
        (d, r) = divMod e 2
        trerec = powermod b t m
        (t, p) = divMod e 3
-}

-- | Zufallszahlenliste fuer die Basiswahl
randomBaseList :: Integer      -- ^ n ist der Primzahlkandidat
              -> IO [Integer] -- ^ die zugehoerige Basisliste
randomBaseList n = do
 gen <- getStdGen
 let rs = randomRs (2, n-1) gen -- Zufallszahlen zwischen 2 und n-1
 return rs

-- RabinMiller für feste Basis a 
singleRabinMillerBool             :: Integer  -- ^ n ist der Primzahlkandidat
                                 -> Integer  -- ^ t kommt von der Zerlegung n=2^t*q
                                 -> Integer  -- ^ e ist der aktuelle Wert von a^((2^e)q)
                                 -> Integer  -- ^ b = a^q
                                 -> Bool     -- ^ True: Moegliche Primzahl, False: keine Primzahl
singleRabinMillerBool n t e b 
     | (b==1)     = True
     | (b==n-1)   = True
     | (e<=t-2)   = singleRabinMillerBool n t (e+1) (b*b `mod` n)  
     | otherwise  = False 


-- | Rabin-Miller
-- | Fuehre 20 Iterationen durch für Fehlerwahrscheinlichkeit < 0.25 ^ 20
listRabinMiller      :: Integer         -- ^ n ist der Primzahlkandidat
                    -> [Integer]       -- ^ die Liste mit den Basen fuer RabinMiller
                    ->  Maybe Integer  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMiller n as
      | (n<3)            = error "N must be greater than 3."
      | (n `mod` 2 == 0) = error "N must be odd."  
      | otherwise        = listRabinMiller2 n as 20


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


-- | Pruefe ob n Primzahl
rabinMillerPrint   :: Integer   -- ^ n ist der Primzahlkandidat
                  -> IO ()     -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPrint n = do
 s <- rabinMillerIO n
 print s

-- | Pruefe ob n Primzahl        
rabinMillerIO         :: Integer            -- ^ n ist der Primzahlkandidat
                   -> IO (Maybe Integer) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerIO n = do
  ls <- randomBaseList n
  let s = listRabinMiller n ls
  return s

-- | Pruefe ob n Primzahl
-- | HAAAACK!
rabinMiller     :: Integer -- ^ n ist der Primzahlkandidat
		-> Maybe Integer -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: moeglicherweise Prim)
rabinMiller n = unsafePerformIO $ rabinMillerIO n
