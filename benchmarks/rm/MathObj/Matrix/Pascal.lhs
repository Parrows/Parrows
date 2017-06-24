Generate a permutated Pascal Matrix.

This is -*- Literate Haskell -*-
\begin{code}
module MathObj.Matrix.Pascal where

import MathObj.Generic.Binomial
import MathObj.Matrix.MatArr
import Data.Array
\end{code}

Eine nicht-permultierte Paskal Matrix B ist

b_{i,j} = (i+j-2)`over`(i-1) fuer i,j\in [1..n]
\begin{code}
pascalList n = [ (i+j)`over`(i) | i<-[0..n-1], j<-[0..n-1] ]

pascalMatArr :: (Ix i, Integral i, Integral n) => i -> MatArr i n
pascalMatArr n = listArray ((1,1),(n,n)) $ pascalList $ fromIntegral n
\end{code}

Wir muessen permutieren!

\begin{codez}
corrupt matrix = let ((_,_),(n,m)) = bounds matrix
                     cs = [ ((i,i), matrix!(i,n-i)) | i<-[1..n] ]
                          ++ [ ((i,n-i), matrix!(i,i)) | i<-[1..n] ]
                 in matrix//cs
\end{codez}

\begin{code}
listPerm n = [ ((i, getJ i n), 1) | i<-[1..n] ]
\end{code}
-- broken
getJ :: Int -> Int -> Int
getJ i n | i`mod`4 == 0 = makeJ i n
--         | (i+1)`mod`4 == 0 = makeJ (i+1) n
         | otherwise = i
    where makeJ i n = n - i
\begin{code}
getJ i n | i `mod` 2 == 0 = i
         | otherwise = n - i
\end{code}
\begin{code}
matPerm n = let xs = listPerm n
                ma = listArray ((1,1),(n,n)) [0,0..]
            in ma//xs

pascalPerm n = let p = matPerm n
                   b = pascalMatArr n
               in p `matMult` b
\end{code}