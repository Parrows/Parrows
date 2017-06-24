This file is -*- Literate Haskell -*-

Matrizen und Vektoren in Arrays. Dabei: keine Instanz von Trans! Es geht nicht!
\begin{code}
module MathObj.Matrix.MatArrCol where

import Data.Array hiding ((!))
import qualified Data.Array as DA
import Data.List
\end{code}

Datenformat. Die Daten werden _spaltenweise_ in Speicher gehalten.
\begin{code}
data MatArrC a b = MA (a, a) (Array a (Array a b))
                   deriving (Eq, Ord)
\end{code}

Konvertieren.
\begin{code}
list2mac :: (Int, Int) -> [b] -> MatArrC Int b
list2mac (n, m) xs = let xss = unshuffleN n xs
                     in lol2mac (n, m) xss

lol2mac :: (Int, Int) -> [[b]] -> MatArrC Int b
lol2mac (n, m) xss = let arrs = map (listArray (1, m)) xss
                     in MA (n, m) $ listArray (1, n) arrs
\end{code}

Hilfscode.
\begin{code}
{- unshuffleN splits a list into n lists
    [takeEach n (drop i xs) | i <- [0..(n-1)]] -}
unshuffleN :: Int -> [a] -> [[a]]
unshuffleN n xs = unshuffle xs
		where  unshuffle xs = map (f xs) [0..n-1]
				where f xs i = g (drop i xs)
				      g [] = []
				      g xs = head xs : (g (drop n xs))

-- simple shuffling (not incremental!)
shuffle :: [[a]] -> [a]
shuffle = concat . transpose
\end{code}

Addressieren
\begin{code}
infix 9 !
mat@(MA (n, m) arr) ! (i, j) | j<=0 || j>m = error "(!): Out of bounds!"
                             | otherwise = let col = getCol mat i
                                           in (DA.!) col j

getCol (MA (n, _) arr) i | i<=0 || i>n = error "getCol: Out of bounds!"
                         | otherwise = (DA.!) arr i
\end{code}