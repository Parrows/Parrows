
Listen mit Laenge, unchunked.

\begin{code}
{-# OPTIONS -fno-monomorphism-restriction #-}
module MathObj.Vector.Indexed where

-- import MathObj.Vector.Base
import qualified Data.List as DL -- (length, transpose)
-- import Prelude (Int, Num, zipWith)
-- import qualified Prelude as P

type Indexed a = (Int, [a])

-- instance Num x => Vectors (x, v) x v where

-- wir brauchen hier Typdefinition!
-- l2 :: Num a => (a -> a -> a) -> (Int, [a]) -> (Int, [a]) -> (Int, [a])
l2 (f) (n, xs) (m, ys) | n/=m = error "List lengths mismatch"
                       | otherwise = (n, zipWith f xs ys) 
l1 (f) (n, xs) = (n, map (f) xs)
length (n, xs) = n
toList (n, xs) = xs
fromList xs = (DL.length xs, xs)
transpose xss = map(fromList) $ DL.transpose $ map (snd) xss
concat xss = (sum $ map (fst) xss, DL.concat $ map (snd) xss)
(!!) (n, xs) k | 0<=k && k<n  -- DL.(!!) xs k
                 = DL.genericIndex xs k
               | otherwise = error "Index not in range!"
splitAt k (n, xs) | 0<=k && k<n = ((k, ls), (n-k, rs))
                  | otherwise = error "Split position not in range!"
    where (ls, rs) = DL.splitAt k xs
filter f (n, xs) = (DL.length res, res)
    where res = DL.filter f xs
(++) (n, xs) (m, ys) = (n+m, (DL.++) xs ys)
\end{code}

Um es bequemer zu machen:

\begin{code}
-- instance Num => Indexed where
--    div = l (P.div)
add = l2 (+)
substr = l2 (-)
mult = l2 (*)
divide = l2 (/)
absolute = l1 (abs)
divideSkalar (n, xs) k = divide (n, xs) (fromList $ take n $ repeat k)
multSkalar (n, xs) k = mult (n, xs) (fromList $ take n $ repeat k)
\end{code}

Und die Kuerzel:
-- werden erst in Vektor.lhs definiert!
\begin{nocode}
(@+) = add
(@-) = substr
(@*) = mult
(@/) = divide
($/) = divideSkalar
\end{nocode}