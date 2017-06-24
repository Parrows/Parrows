Listen mit Laenge, chunked.

Momentan ist es nur ein Hilfstyp, wir arbeiten mit chunked vektoren und wenn's zur uebertragung kommt, chunken wir. Man kann auch die cLiftN funktionen verwenden.

\begin{code}
module MathObj.Vector.Chunked (chunk, unchunk, cLift1, cLift2, cLift1N, cLiftN1) where

-- import MathObj.Vector.Base
import qualified Data.List as DL -- (length, transpose)
-- import Prelude (Int, Num, zipWith)
-- import qualified Prelude as P
import Prelude
import MathObj.Vector.Chunking (chunk, unchunk)
-- import MathObj.Vector.Indexed

type Chunked a = (Int, [[a]])
type Unchunked a = (Int, [a])
\end{code}


Wir liften eine funktion auf die chunks
\begin{code}
cLift1 :: Num a => Int -> (Unchunked a -> Unchunked a) -> Chunked a -> Chunked a
cLift1 k f xss = chunk k $ f $ unchunk xss

cLift2 :: Num a => Int -> (Unchunked a -> Unchunked a -> Unchunked a) 
       -> Chunked a -> Chunked a -> Chunked a
cLift2 k f xss yss = chunk k $ f (unchunk xss) (unchunk yss)

cLift1N :: Num a => Int -> (Unchunked a -> [Unchunked a]) 
           -> Chunked a -> [Chunked a]
cLift1N k f xss = map (chunk k) $ f $ unchunk xss

cLiftN1 :: Num a => Int -> ([Unchunked a] -> Unchunked a)
           -> [Chunked a] -> Chunked a
cLiftN1 k f xsss = chunk k $ f $ map (unchunk) xsss
\end{code}
\begin{nocode}
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
\end{nocode}

Um es bequemer zu machen:

\begin{nocode}
-- instance Num => Indexed where
--    div = l (P.div)
add = l2 (+)
substr = l2 (-)
mult = l2 (*)
divide = l2 (/)
absolute = l1 (abs)
divideSkalar (n, xs) k = divide (n, xs) (fromList $ take n $ repeat k)
multSkalar (n, xs) k = mult (n, xs) (fromList $ take n $ repeat k)
\end{nocode}

Und die Kuerzel:
-- werden erst in Vektor.lhs definiert!
\begin{nocode}
(@+) = add
(@-) = substr
(@*) = mult
(@/) = divide
($/) = divideSkalar
\end{nocode}