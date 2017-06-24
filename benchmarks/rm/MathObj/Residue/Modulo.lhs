Unimodulare Darstellung
\begin{code}
{-# OPTIONS -cpp #-}
module MathObj.Residue.Modulo where

import MathObj.Residue.Inverse
import MathObj.Residue.Euclid (eea)
#ifdef FERMAT_POWER
import MathObj.Residue.Power
#endif

import Prelude -- hiding ((+),(-),(*),(/))
import qualified Prelude as P
\end{code}

\begin{code}
data (Ord a, Eq a, Show a) => Mod a = Z a a
                                      deriving (Show, Eq, Ord)
\end{code}
TODO: Eigenes show, das "a mod p" zeigt.


Eine Entkoppelung mit Integral a, Integral b ist wegen makeFZ notwendig, dort ist die Eingabe auch Fractional, die Ausgabe aber nicht.
\begin{code}
makeZ :: (Integral a) => a -> a -> Mod a
-- makeZ a p = Z (a `mod` p) p
makeZ a p = let m = a `mod` p
                o = if m<0 
                    then m+p
                    else m
            in Z o p

lift2 :: Integral a => (a -> a -> a) -> Mod a -> Mod a -> Mod a
lift2 f (Z a p) (Z b q) | p /= q = error "Different residue classes!"
                        | otherwise = makeZ (f a b) p

instance (Integral a) => Num (Mod a) where
--    (+), (-), (*), (/) :: (Integral a) => Mod a -> Mod a -> Mod a
    (+) = lift2 (P.+)
    (-) = lift2 (P.-)
    (*) = lift2 (P.*)
    fromInteger = errorBreak "fromInteger in Modulo module: no residue class specified!"
    abs = error "abs: it's a residue class"
    signum = error "signum: it's a classic residue class"
#ifdef FERMAT_POWER
pow :: (Integral a) => Mod a -> a -> Mod a
pow (Z a p) n = flip makeZ p (powM a n p)
#endif

-- allow breakpoints
errorBreak = error

toIntegral :: Integral a => Mod a -> a
toIntegral (Z a _) = a

makeZfromZ :: Integral a => a -> Mod a -> Mod a
makeZfromZ a (Z _ m) = makeZ a m
\end{code}

Nun zu Division!
\begin{xcode}
(/) (Z a p) (Z b q) | p /= q = error "(/): Different residue classes!"
                    | otherwise = a * c
                    where c = flip makeZ p $ inverse b p
\end{xcode}

Besser:
\begin{code}
instance (Integral a) => Fractional (Mod a) where
    (/) (Z a p) (Z b q) | p /= q = error "(/): Different residue classes!"
                        | otherwise = let ((x, _), (c, _)) = eea ((b, p), (a, 0))
                                      in if x/= 1 then error "x != 1"
                                         else makeZ c p
    fromRational = error "no residue class specified!"

multiplicativeInverse :: Integral a => Mod a -> Mod a
multiplicativeInverse (Z x p) = flip makeZ p $ inverse x p
\end{code}