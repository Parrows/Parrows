Partitionen der Listen. Werden in unterschiedlichen Varianten von `divide` verwendet.

Wir nutzen hier *nur* Funktionen aus MathObj.Vector.Vector, wodurch wir uns automatisch auf die unterliegende Strunktur anpassen koennen.

\begin{code}
module MathObj.Vector.Partitions where
import qualified Prelude -- hide standard implementations
import Prelude (div, mod, odd, even, (+), (-), (==), ($), error, (.))
import Data.List as DL (filter, zipWith, map)
import MathObj.Vector.Vector
\end{code}

Links-Rechts Aufteilung
\begin{code}
splitHalves xs = splitAt (length xs `div` 2) xs
\end{code}
%Ach ja, und wir brauchen Listen und keine Tupel:
%\begin{code}
%splitHalves xs = [ls, rs]
%    where (ls, rs) = splitHalves' xs
%\end{code}

Odd-Even Aufteilung
\begin{code}
filterIndex f xs = fromList [xs!!i | i<-DL.filter f [0..length xs - 1] ]
splitOddEven xs = [filterIndex even xs, filterIndex odd xs]
\end{code}

Nun, fuer r-radix:
\begin{code}
--splitParts :: Prelude.Int -> [a] -> [[a]]
splitParts 1 xs = [xs]
splitParts n xs = ys : splitParts (n-1) zs
    where (ys, zs) = splitAt (length xs `div` n) xs
-- splitParts _ _ = error "Partition index mismatch!"


splitR r xs = transpose $ splitParts (length xs `div` r) xs
splitN = splitParts


splitIntoN :: Prelude.Int -> [a] -> [[a]]
splitIntoN n xs = takeIter parts xs
  where l = Prelude.length xs
        parts = Prelude.zipWith (+) ((Prelude.replicate (l `mod` n) 1) Prelude.++ Prelude.repeat 0) 
                             (Prelude.replicate n (l `div` n))

takeIter :: [Prelude.Int] -> [a] -> [[a]]
takeIter [] [] = []
takeIter [] _ = error "elements left over"
takeIter (t:ts) xs = hs : takeIter ts rest
  where (hs,rest) = Prelude.splitAt t xs

unshuffle :: Prelude.Int -> [a] -> [[a]]
unshuffle n xs = [takeEach n (Prelude.drop i xs) | i <- [0..n-1]]

takeEach :: Prelude.Int -> [a] -> [a] 
takeEach n [] = []
takeEach n (x:xs) = x : takeEach n (Prelude.drop (n-1) xs)

unshuffle' :: Prelude.Int -> (Prelude.Int, [a]) -> [(Prelude.Int, [a])]
unshuffle' n (len,xs) = Prelude.zip parts [takeEach n (Prelude.drop i xs) | i <- [0..n-1]] 
  where parts = Prelude.zipWith (+) ((Prelude.replicate (len `mod` n) 1) Prelude.++ Prelude.repeat 0) 
                                     (Prelude.replicate n (len `div` n))


-- inverse to unshuffle
-- shuffle :: [[a]] -> [a]
-- shuffle = Prelude.concat . DL.transpose

\end{code}