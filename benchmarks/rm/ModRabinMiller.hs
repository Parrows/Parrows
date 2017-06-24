{-# OPTIONS -cpp #-}
module ModRabinMiller where 
-- Original Autor: Kent Kwee
-- Modifications by Oleg Lobachev

import System.Random

import System.IO.Unsafe (unsafePerformIO) -- HAAACK!

import MathObj.Residue.IntMulti
import MathObj.Residue.Modulo
import IntMultiLift
import Prelude hiding (div, mod, divMod)

-- type MyInt = Integer -- so war das
type MyInt = (Mod Int) -- a single residue. Now we can map!

-- Hilfsfunktionen
	   
-- | Zerlege n in gerade und ungerade Anteile
-- zerlege               :: (MyInt,MyInt)   -- ^ Eingabe (n,0)
--                      -> (MyInt,MyInt)  -- ^ Ausgabe (q,t) mit n=2^t*q
zerlege' :: (Int, Int) -> (Int, Int)
zerlege' (q, t) 
  | (r == 0) = zerlege' (b, t+1)
  | otherwise        = (q,t)
  where (b, r) = q `divMod` (2 :: Int)

zerlege :: (MyInt, MyInt) -> (MyInt, MyInt)
zerlege ((Z q qm), (Z t tm))
    | qm /= tm = error "zerlege: different residues!"
    | otherwise = (makeZ qr qm, makeZ tr tm)
    where (qr, tr) = zerlege' (q, t)

-- -- | Modulares Potenzieren: b^e mod m
-- -- | TODO: A faster powering algorithm
-- powermod             :: Integer    -- ^ b ist die Basis
--                     -> Integer    -- ^ e ist der Exponent
--                     -> Integer    -- ^ m der Modul
--                     -> Integer    -- ^ Das Ergebnis b^e mod m
-- powermod b e m 
--   | (e==0)           = 1
--   | (e `mod` 2 == 0) = (temp * temp) `mod` m 
--   | otherwise        = b * (powermod b (e-1) m) `mod` m 
--   where temp = (powermod b (e `div` 2) m) 

powermod'             :: Int    -- ^ b ist die Basis
                      -> Int    -- ^ e ist der Exponent
                      -> Int    -- ^ m der Modul
                      -> Int    -- ^ Das Ergebnis b^e mod m
powermod' b e m 
  | (e==0)           = 1
  | (e `mod` 2 == 0) = (temp * temp) `mod` m 
  | otherwise        = b * (powermod' b (e-1) m) `mod` m 
  where temp = (powermod' b (e `div` 2) m) 


-- | Modulares Potenzieren: b^e mod m
-- | TODO: A faster powering algorithm
powermod             :: MyInt    -- ^ b ist die Basis
                     -> MyInt    -- ^ e ist der Exponent
                     -> MyInt    -- ^ n der Modul
                     -> MyInt    -- ^ Das Ergebnis b^e mod n
powermod (Z b bm) (Z e _) (Z n nm) 
    | bm /= nm = error "powermod: wrong residues!"
    | otherwise = makeZ res bm
    where res = powermod' b e n
        

-- | Zufallszahlenliste fuer die Basiswahl
randomBaseList :: MyInt      -- ^ n ist der Primzahlkandidat
               -> IO [MyInt] -- ^ die zugehoerige Basisliste
randomBaseList (Z n m) = do
 gen <- getStdGen
 let rs = randomRs (2, n-1) gen -- Zufallszahlen zwischen 2 und n-1
     ms = map (\r -> Z r m) rs
 return ms

-- RabinMiller für feste Basis a 
singleRabinMillerBool             :: MyInt  -- ^ n ist der Primzahlkandidat
                                  -> MyInt  -- ^ t kommt von der Zerlegung n=2^t*q
                                  -> MyInt  -- ^ e ist der aktuelle Wert von a^((2^e)q)
                                  -> MyInt  -- ^ b = a^q
                                  -> Bool     -- ^ True: Moegliche Primzahl, False: keine Primzahl
singleRabinMillerBool n t e b 
     | (b==one)     = True
     | (b==n-one)   = True
     | (e<=t-two)   = singleRabinMillerBool n t (e+one) (b*b `mod` n)  
     | otherwise  = False 
     where one = makeZfromZ 1 n
           two = makeZfromZ 2 n


-- | Rabin-Miller
-- | Fuehre 20 Iterationen durch für Fehlerwahrscheinlichkeit < 0.25 ^ 20
listRabinMiller      :: MyInt         -- ^ n ist der Primzahlkandidat
                     -> [MyInt]       -- ^ die Liste mit den Basen fuer RabinMiller
                     ->  Maybe MyInt  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
#warning "Skipping intermediate checks"
listRabinMiller n as  = listRabinMillerP2 n as 20

-- | RabinMiller für feste Basen a aus einer MyInt-liste
listRabinMiller2 :: MyInt        -- ^ n ist der Primzahlkandidat
                 -> [MyInt]      -- ^ die Liste mit den Basen fuer RabinMiller
                 -> Int          -- ^ Restliche Durchlaeufe
                 -> Maybe MyInt  -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
listRabinMiller2 n (a:as) c
     | (c==0)                          = Just n
     | (singleRabinMillerBool n t 0 b) = listRabinMiller2 n as (c-1) 
     | otherwise                       = Nothing
     where ((q,t),b) = (zerlege(n-1,0) , powermod a q n)    

-- von OL
-- A more generic farmUntil.
mapUntil :: -- (Trans a, Trans b, Trans c) =>
            ((a -> b) -> [a] -> [b]) ->    -- ^ map
            ([b] -> c) ->                  -- ^ reduce
            (a -> b) ->                    -- ^ worker function
            [a] -> c
mapUntil amap areduce f xs = areduce $ amap f xs -- meh.


-- von OL
listRabinMillerP2 :: MyInt -> [MyInt] -> Int -> Maybe MyInt
listRabinMillerP2 n as k = let f :: (MyInt, MyInt) -> Bool
                               f (n, a) = singleRabinMillerBool n t zero b
                                   where ((q,t),b) = (zerlege(n - one, zero) , powermod a q n)
                                         one = makeZfromZ 1 n
                                         zero = makeZfromZ 0 n
                               tasks = take k $ [(n, a) | a<-as]
                               reduce :: MyInt -> [Bool] -> Maybe MyInt
                               reduce n bs | and bs = Just n
                                           | otherwise = Nothing
                           in mapUntil map (reduce n) f tasks


-- | Pruefe ob n Primzahl
rabinMillerPrint   :: MyInt   -- ^ n ist der Primzahlkandidat
                  -> IO ()     -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerPrint n = do
 s <- rabinMillerIO n
 print s

-- | Pruefe ob n Primzahl        
rabinMillerIO         :: MyInt            -- ^ n ist der Primzahlkandidat
                   -> IO (Maybe MyInt) -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: Moegliche Primzahl) 
rabinMillerIO n = do
  ls <- randomBaseList n
  let s = listRabinMiller n ls
  return s

-- | Pruefe ob n Primzahl
-- | HAAAACK!
rabinMiller     :: MyInt -- ^ n ist der Primzahlkandidat
		-> Maybe MyInt -- ^ Die Ausgabe (Nothing: keine Primzahl, Just n: moeglicherweise Prim)
rabinMiller n = unsafePerformIO $ rabinMillerIO n
