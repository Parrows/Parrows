Rationales multimodulares Rechnen.

Nach der Diplomarbeit von OL.
\begin{code}
{-# OPTIONS -XTypeSynonymInstances -cpp #-}
\end{code}
CPP Flags:

GK_WRONG: mappings as in Gregory&Krishnamurthy'84, not so good
default: mappings as in Lobachev'07, better

Fuer ein Moment:
\begin{code}
-- #define GK_WRONG
\end{code}

\begin{code}
module MathObj.Residue.FracMulti ((+), (-), (*), (/), 
                                  makeFZ, makeFZ', 
                                  restoreFZ', restoreFZ, restoreFZmaybe,
                                  mFromN, nFromM, getM, getN,
                                  FSingleMod, FMods
                                 ) where

import Prelude
import MathObj.Residue.IntMulti hiding (lift2)
import MathObj.Residue.Modulo hiding (lift2)
import qualified Prelude as P
import qualified MathObj.Residue.IntMulti as IM
import qualified MathObj.Residue.Modulo as M
import Data.List
import MathObj.Residue.Inverse
import MathObj.Residue.Euclid
import MathObj.Generic.VulgarFraction

-- import Debug.Trace
trace _ x = x
\end{code}

Wir implementieren nur die "richtige" Abbildungen samt Arithmetik.

Datentyp: 

in der Arbeit: (u, v) mod p, hier: [FZ u v p]
anderweitig: [FM (Z u p) v]
noch anders: FS IZ us ps [v]
\begin{code}
data (Ord a, Eq a, Show a) => FSingleMod a = FZ a a a | FM (Mod a) a
                                             deriving (Eq, Show, Ord)
type FMods a = [FSingleMod a]
\end{code}

Ich weiss noch nicht was bequemer wird, aber es laesst sich ja einfach konvertieren
\begin{code}
z2m :: Integral a => FSingleMod a -> FSingleMod a
z2m (FM (Z u p) v) = FZ u p v
z2m x = x
m2z ::  Integral a => FSingleMod a -> FSingleMod a
m2z (FZ u p v) = FM (Z u p) v
m2z x = x
zs2ms = map z2m
ms2zs = map m2z
\end{code}

Die Dimension des Systems.
M = p_1*...*p_n
N = \sqrt(M/2)
\begin{code}
primes :: Integral a => FMods a -> [a]
primes = map (\(FM (Z _ p) _) -> p)

getM, getN :: (Integral i) => FMods i -> i
getM = product . primes
getN = nFromM . getM
nFromM, mFromN :: (Integral i) => i -> i
nFromM = truncate . sqrt . flip (/) 2 . fromIntegral
mFromN = (*) 2 . flip (^) 2
\end{code}

Vorwaertsabbildung

DAS IST DIE FALSCHE!
\begin{code}
#ifdef GK_WRONG
-- makeFZ' :: (Fractional f, Integral f, Integral i) =>
--           f -> [i] -> FMods i -- eingabe / primzahlen / ergebnis
makeFZ' a b ps | gcd a b == 1 = let (y, ws) = extractFactors a ps
                                    (z, qs) = extractFactors b ps
                                    vs = zipWith (-) ws qs -- es ist ja gekuerzt!
                                    cs = convertFractions y z ps
                                    -- in zipWith (FM) (makeIZ cs ps) vs
                                in zipWith FM (cs) vs
               | otherwise = let c = gcd a b
                             in makeFZ' (a`div`c) (b`div`c) ps
                                    -- kuerzen!

 -- stub
 -- extractFactors _ _ = [0,0..]
\end{code}

 Achtung! Genau hier liegt der Unterschied zwischen richtig und falsch!
\begin{code}
extractFactors x [] = (x, [])
extractFactors x (p:ps) = let (power, y) = detectPower x p
                              (z, list) = extractFactors y ps
                          in (z, power:list)
#else
\end{code}

Hier kommt die richige Abbildung! Richtige?

Wir ziehen die Faktoren nur aus einem Element, nicht aus allen!

\begin{code}
-- extractFactors :: (Num t, Integral p, Integral v) => t -> p -> [(t, v)]
extractFactors x ps = map (detectPower x) ps
makeFZ' a b ps | gcd a b == 1 = let (ws, ys) = unzip $ extractFactors a ps
                                    (qs, zs) = unzip $ extractFactors b ps
                                    vs = zipWith (-) ws qs -- es ist ja gekuerzt!
                                    cs = zipWith3 convertFraction ys zs ps
                                    -- cs = trace ("cs: " ++ (show ys) ++ (show zs) ++ (show ps)) $ zipWith3 convertFraction ys zs ps
                                in zipWith FM (cs) vs
               | otherwise = let c = gcd a b
                             in makeFZ' (a`div`c) (b`div`c) ps


makeFZ (F a b) = makeFZ' a b
#endif
\end{code}
\begin{code}
 -- detectPower :: Integral a => a -> a -> (a, a)
detectPower x p = let worker (x, p, m) = let (d, m') = divMod x p
                                         in (d, p, m')
                      listFactors = takeWhile (\(_, _, m) -> m==0) $ iterate (worker) (x, p, 0)
                      finalD = (\(d, _, _) -> d) $ last' listFactors
                      last' [] = (x, p, 0)
                      last' xs = last xs
                  in (genericLength listFactors - 1, finalD)

 -- convertFractions :: (Fractional f, Integral p, Integral i) => f -> [p] -> [i]
 -- convertFractions _ _ _ = [0,0..]
convertFractions x y = map (convertFraction x y)

convertFraction :: (Integral i) => i -> i -> i -> Mod i
convertFraction x y p = let ((d,_),(r,_)) = eea ((p,y),(0,x))
                        in if d/= 1 
                           then error $ "convertFraction: " 
                                    ++ "[ " ++ (show p) ++ ", " ++ (show y) ++ "; 0, " ++ (show x) ++ " ]"
                           else makeZ r p
\end{code}

\begin{code}
instance (Integral a) => Num (FSingleMod a) where
    (+) x y = addSingle x y
    (-) x y = x + (additiveInverseSingle y)
    (*) (FM u1 v1) (FM u2 v2) = FM (u1*u2) (v1+v2)

instance (Integral a) => Fractional (FSingleMod a) where
    (/) x y = x * (multiplicativeInverseSingle y)
    fromRational = error "please use the multimodulary version!"
\end{code}

16-2+6 Faelle fuer die Addition!
\begin{code}
-- stub
-- addSingle _ _ = error "it's complicated"
addSingle :: (Integral a) => FSingleMod a -> FSingleMod a -> FSingleMod a
addSingle (FM (Z 0 p) _) (FM (Z 0 p') _) | p==p' 
                                             = FM (Z 0 p) 0 -- 1
addSingle (FM (Z 0 p) _) y = y                              -- 3
addSingle x (FM (Z 0 p) _) = x                              -- 3
addSingle (FM u 0) (FM u' 0) = FM (u+u') 0                  -- 2
addSingle (FM u v) (FM u' 0)  | v >0  = FM u' 0             -- 1
                              | v <0  = FM u v              -- 1
addSingle (FM u 0) (FM u' v') | v'>0  = FM u 0              -- 1
                              | v'<0  = FM u' v'            -- 1
addSingle (FM u v) (FM u' v') | v<v'  = FM u v              -- 2
                              | v>v'  = FM u' v'            -- 2
                              | v==v' = FM (u+u') v         -- 2
addSingle _ _ = error "Bad case in addSingle"               -- 0
                              --                            // 19. ok??        
\end{code}

\begin{code}
-- additiveInverseSingle _ = error "it's complicated"
additiveInverseSingle (FM (Z u p) v) = FM (Z (p-u) p) v
multiplicativeInverseSingle (FM u v) = (FM (M.multiplicativeInverse u) (-v))
\end{code}

Jetzt multimodular!
\begin{code}
instance (Integral a) => Num (FMods a) where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
instance (Integral a) => Fractional (FMods a) where
    (/) = zipWith (/)
\end{code}

Und zurueck!
\begin{code}
-- restoreFZ  :: Integral a => FMods a -> (a, a) -- TODO: replace with Fraction
restoreFZpair  x = let unmaybe (Just x) = x
                       unmaybe Nothing  = error "restoreFZ: no fraction could be found. Out of bounds?"
                       (mr, (nom, denom)) = restoreFZ' x
                       (n', d') = unmaybe mr
                   in (nom*n', denom*d')
restoreFZ x = let (a, b) = restoreFZpair x
              in a/:/b
-- restoreFZmaybe :: (Num a, Integral a) => FMods a -> Maybe (Fraction a)
restoreFZmaybe x = let r = restoreFZ' x
                       fmaybe :: (a -> b -> c) -> (Maybe a, b) -> Maybe c
                       fmaybe f ((Just x), y) = Just (f x y)
                       fmaybe _ (Nothing, _) = Nothing
                       ausmult :: Integral a => (a, a) -> (a, a) -> Fraction a
                       ausmult (a, b) (c, d) = (a*c) /:/ (b*d)
                   in fmaybe (ausmult) r
\end{code}

Wieder mal nach G&K!
\begin{code}
#ifdef GK_WRONG
-- restoreFZ' :: Integral a => FMods a -> ((Maybe (a, a)), (a, a)) -- TODO: replace with Fraction!
restoreFZ' x = let m = getM x
                   n = getN x
                   (y, (nom, denom)) = stripPowers x
                   z = convertToIntResidues y
                   r = toIntegral $ restoreIZ z
                   e = eeaSearch ((m, r), (0, 1)) n
               in (e, (nom, denom))

stripPower :: Integral a => FSingleMod a -> ((FSingleMod a), a, a)
stripPower (FM (Z u p) v) | v > 0 = ((FM (Z u p) 0), p^v, 0)
                          | v < 0 = ((FM (Z u p) 0), 0, p^(-v))
                          | otherwise =  ((FM (Z u p) 0), 0, 0)
                                
stripPowers :: Integral a => FMods a -> (FMods a, (a, a)) 
stripPowers xs = let f1 = map (\(x, _, _) -> x)
                     f2 = foldr (\((FM _ p), nom, denom) (nom', denom') 
                                     -> (p^nom * nom', p^denom * denom')) (1, 1)
                     y = map (stripPower) xs
                 in (f1 y, f2 y)
\end{code}

Nach Lobachev'07:
[(u1, v1), (u2, v2), (u3, 0)] -> ([(u1/v2, 0), (u2/v1, 0), (u3/(v1*v2), 0)], v1*v2)
FMods a -> (FMods a, Nominator, Denominator)
\begin{code}
#else
\end{code}
divideAllButSome indexNotToDivide divideBy [(Index, FSingleMod)]
\begin{code}
divideAllButSome1 :: (Integral a, Ord a) => Int -> a -> a -> [(Int, FSingleMod a)] -> [(Int, FSingleMod a)]
divideAllButSome1 i a b xs = let filtered = filter (\(j, _) -> j/=i) xs
                                 kMapped = makeFZ' a b $ primes $ map (snd) filtered
                                 semiResults = zipWith (\(j, x) y -> (j, x/y)) filtered kMapped
                                               -- als Ergebnis sollen alle v==0 sein!
                                 -- spared = xs!!i
                                 -- removeV :: (Int, FSingleMod a) -> (Int, FSingleMod a)
                                 removeV (i, (FM u v)) -- = (i, (FM u 0))
                                     = trace ("removeV from " ++ (show u) ++ ", " ++ (show v) )
                                       $ (i, (FM u 0))
                                 spared = removeV $ xs!!i
                                 results = spared:semiResults
                                 -- die Reinfolge ist eigentlich unwichtig, aber...
                                 -- in map snd $ sortBy (\(x,_) (y,_) -> compare x y) results
                             in results

divideAllButSome _ a b xs = let ys = map snd xs
                                ds = makeFZ' a b $ primes ys
                                res = zip [0..] (ys / ds)
                            in trace ("divide: " ++ show res) $ res
\end{code}
foo x y z = let a = makeFZ' x y z
                b = makeFZ' y x z
            in compare a b
\begin{code}
isNotZeroV :: Integral a => FSingleMod a -> Bool
isNotZeroV (FM _ 0) = False
isNotZeroV _ = True

iteratorFraction :: Integral a => FMods a -> [a] -> [a] -> ([a], [a])
iteratorFraction ((FM (Z u p) v):rs) xs ys | v > 0 = iteratorFraction rs ((p^v):xs) ys
                                           | v < 0 = iteratorFraction rs xs ((p^(-v)):ys)
                                           | otherwise = error "iteratorFraction: v==0!"
iteratorFraction _ xs ys = (reverse xs, reverse ys) 

-- stripPowers :: (Integral a, Ord a) => [FSingleMod a] -> [FSingleMod a]
stripPowers xs = let indexed = zip [0..] xs
                     notZeros = filter (\(_, v) -> isNotZeroV v) indexed
                     (zs1, zs2) = unzip notZeros
                     (noms, denoms) = iteratorFraction zs2 [] []
                     {-
                     results = map (\(i, nom, denom) -> divideAllButSome i nom denom notZeros) 
                               $ zip3 zs1 noms denoms
                     -- sortResults = sortBy (\(x, _) (y, _) -> compare x y) results
                      -}
                     -- iterator [] [] [] x = x
                     iterator [] _ _ x = x
                     iterator _ [] [] x = x
                     iterator a [] c x = iterator a [1] c x
                     iterator a b [] x = iterator a b [1] x
                     iterator (a:as) (b:bs) (c:cs) x = let r = divideAllButSome a b c x
                                                           -- in iterator as bs cs r
                                                       in trace ("iterator: " ++ (show a) ++ ", " 
                                                              ++ (show b) ++ ", " ++ (show c) ++ ", " 
                                                              ++ (show r) )
                                                              $ iterator as bs cs r
                     iterator a b c x = error $ "Oops in iterator: " ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ ", " ++ (show x)
                     results = trace ("results: " ++ (show zs1) ++ ", " ++ (show noms) ++ "/" ++ (show denoms) ++ ", " ++ (show indexed) ) $ iterator zs1 noms denoms indexed
                     -- iterator zs1 noms denoms indexed
                     stripResults =  map snd $ sortBy (\(x,_) (y,_) -> compare x y) results
                 in (product noms, product denoms, stripResults) 
\end{code}
-- die falsche
restoreFZ' x = let m = getM x
                   n = getN x
                   (y, (nom, denom)) = stripPowers x
                   z = convertToIntResidues y
                   r = toIntegral $ restoreIZ z
                   e = eeaSearch ((m, r), (0, 1)) n
               in (e, (nom, denom))
\begin{code}
restoreFZ' :: Integral i => FMods i -> (Maybe (i,i), (i,i))
restoreFZ' x = let m = getM x
                   n = getN x
                   (nom, denom, strips) = stripPowers x
                   z = convertToIntResidues strips
                   r = toIntegral $ restoreIZ z
                   e = eeaSearch ((m, r), (0, 1)) n
               in (e, (nom, denom))
#endif
\end{code}
\begin{code}
convertToIntResidue (FM (Z u p) 0) = Z u p
convertToIntResidue _ = error "convertToIntResidiue: non-zero residue power"
convertToIntResidues :: Integral a => FMods a -> IMods a
convertToIntResidues = map convertToIntResidue
\end{code}
               