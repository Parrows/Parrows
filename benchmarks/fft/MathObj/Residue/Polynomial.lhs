{-# OPTIONS -fno-implicit-prelude #-}

Polynomielle Reste.

Liste `mod` Liste = Liste

\begin{code}
module MathObj.Residue.Polynomial where

import MathObj.Vector.Vector hiding ((++))
import Prelude hiding (length, (!!), concat, div, mod, divMod)
import qualified Prelude as P (div, (++))
\end{code}

Aus numeric Prelude:

divMod :: (ZeroTestable.C a, Field.C a) => [a] -> [a] -> ([a], [a])
divMod x y =
    let (y0:ys) = dropWhile isZero (reverse y)
        aux l xs' =
          if l < 0
            then ([], xs')
            else
              let (x0:xs) = xs'
                  q0      = x0/y0
                  (d',m') = aux (l-1) (sub xs (scale q0 ys))
              in  (q0:d',m')
        (d, m) = aux (length x - length y) (reverse x)
    in  if isZero y
          then error "MathObj.Polynomial: division by zero"
          else (reverse d, reverse m)

Die Division
\begin{code}
-- type Poly = Num a => (Int, [a])
-- type Monom = Num a => (Int, a)

-- divHelper :: Poly -> Monom -> Poly
divHelper (n, xs) (m, k) | n<m = (0, []) -- hier kommt noch der rest!
                         | k==0 = error "division by zero"
                         | otherwise = 
                             let res = toList $ (n, xs) $/ k
                             in (n `P.div` m, res)

divMod (n, xs) (m, ys) = (zs, rs)
    where rs = (n, xs) @- ((m, ys) @* zs)
          zs = div' (n, xs) (m, ys)
          div' (n, xs) (m, y:ys) | m>1 = 
                                     let diff = pad n $ divHelper (n, xs) (m, y)
                                         newxs = (n, xs) @- diff
                                     in div' newxs (m-1, ys)
                                 | otherwise = divHelper (n, xs) (m, y)

-- div = fst $ divMod
-- mod = snd $ divMod
\end{code}

Padding.
\begin{code}
pad k (n, xs) | k>n = (k, take (k-n) $ repeat 0 P.++ xs)
              | otherwise = error "Negatives padding!"
\end{code}