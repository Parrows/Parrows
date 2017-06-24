Gauss Elimination

This is -*- Literate Haskell -*-

\begin{code}
{-# OPTIONS -cpp #-}
module MathObj.Matrix.Gauss where

import MathObj.Matrix.MatArr
import Data.Array ((!), (//), bounds, Ix, listArray)
import MathObj.Matrix.Residue
import MathObj.Generic.VulgarFraction (Fraction, maxFarey)



#define __EDEN__ 1
#ifdef __EDEN__
import Parallel.Eden
#endif

-- for interactive mode
import MathObj.Generic.VulgarFraction ((/:/))

import Debug.Trace (trace)
-- trace _ = id
\end{code}
Straightforward Gauss:

        n=length(A)

        for i=1:n
                for j=i+1:n
                        y=A(j, i)/A(i, i);
                        A(j, i)=y;
                        A(j, i+1:n)=A(j, i+1:n)-y*A(i, i+1:n);
                end
        end

\begin{code}
gaussIterator (i, bound, matrix) = let ((ln, lm), (n, m)) = bound                                       
                                       zs = [ ((j, k), makeEl i j k) 
                                              -- let y = matrix!(j, i)/matrix!(i, i)
                                                  | j<-[i+1..n], k<-[i..n] ]
                                       makeY (i, j) = matrix!(j, i)/matrix!(i, i)
                                       makeEl i j k = matrix!(j, k)-makeY(i, j)*matrix!(i,k)
                                       result = (i+1, bound, matrix//zs)
                                       -- in trace ("iterator: a[i+1,i]=" ++ (show $ matrix!((i+1),i)) 
                                       --           ++ ", y[" ++ (show (i+1)) ++ "]=" ++ (show $ makeY (i,(i+1))) 
                                       --           ++ ", i=" ++ (show i) ++ "\n"
                                       --           ++ " makeEl i (i+1) (i+1)=" ++ (show $ makeEl i (i+1) (i+1))
                                       --           ++ ", zs=" ++ (show zs) ++ ", before a=" ++ (showMat matrix)) 
                                       --        $ result
                                   in result

gauss :: (Num x, Fractional x) => MatArr Int x -> MatArr Int x
gauss matrix = let ((ln, lm), (n, m)) = bounds matrix
                   (_, _, result) = last $ take (n-ln+1) 
                                    $ iterate gaussIterator (ln, ((ln, lm), (n, m)), matrix)
               in result
\end{code}

\begin{code}
diag matrix = let ((ln, lm), (n, m)) = bounds matrix
              in [ matrix!(i,i) | i<-[ln..n] ]
\end{code}
\begin{seqcode}
det :: (Num x, Fractional x) => MatArr Int x -> x
det = product . diag . gauss
\end{seqcode}

Now for something completly different!

\begin{seqcode}
gaussResidue = lift1pm gauss

detResidue ps x = let y = gaussResidue ps x
                  in product $ map unmaybe $ diag $ y
\end{seqcode}

Parallele Version.
\begin{code}
unmaybe (Just x) = x
unmaybe Nothing = error "Nothing an der falschen Stelle!"
{-
#ifdef __EDEN__
gaussResidueG :: (Trans a, Trans n, Integral n, Num a) =>
--                 ((a -> b) -> [a] -> [b])
                 ((TransMat Int a -> TransMat Int a) 
                      -> [TransMat Int a] -> [TransMat Int a])
              -> [n] 
              -> MatArr Int (Fraction n) 
              -> MatArr Int (Maybe (Fraction n))
#endif
-}
-- gaussResidueG :: blah
gaussResidueG myMap = lift1gpm myMap gauss
-- gaussResidueGC :: Int -> blah
gaussResidueGC c myMap = lift1gpmc c myMap gauss
-- gaussResidueGCD myMap = lift1gpmcd c myMap gauss -- returns only diagonals
gaussResidueGCD c myMap = lift1tss c myMap gauss -- returns only diagonals
-- gaussResidueGCL :: (Trans a, Trans b, Trans c, Num c, Trans d, Num d) => 
--                    Int -> Map a b -> [d]
--                 -> MatArr Int c -> MatArr Int (Maybe c)
gaussResidueGCL c myMap = lift1tsl c myMap gauss -- returns only diagonals
gaussResidueGCD2 c myMap = lift1tss2 c myMap gauss -- returns only diagonals
{-
#ifdef __EDEN__
detResidueG :: (Trans a, Trans n, {- Trans i, 
                Ix i, Integral i, -} Integral n, Num a) =>
            -- ((a -> b) -> [a] -> [b])
           ((TransMat Int a -> TransMat Int a) 
                -> [TransMat Int a] -> [TransMat Int a])
            -> [n]
            -> MatArr Int (Fraction n)
            -> Fraction n
#endif
-}
-- instance (NFData i, Integral i) => NFData (Fraction i)

detResidueG myMap ps x = let y = trace ("Calling Gauss...") (gaussResidueG myMap ps x)
                             dy = diag y
                             (a, b) = countJust dy 0 0
                             countJust [] a b = (a, b)
                             countJust ((Just _):xs) a b = countJust xs (a+1) b
                             countJust (Nothing:xs) a b = countJust xs a (b+1)
                             d = trace ("Diag: " ++ (show a) ++ " Justs, " 
                                        ++ (show b) ++ " Nothings") $ 
                                 d1
                             d1 = map unmaybe $ dy
                             res = rnf d `seq` product d
                             -- in res 
                             bound = maximum $ map maxFarey d
                         in trace ("Computed bound: " ++ (show bound))
                                $ res
\end{code}
\begin{type}
type Map a = (a -> a) -> [a] -> [a]
detResidueGC :: (Trans a, Trans x, Num x) =>
                Int -> Map a -> [x] -> MatArr Int x -> x
\end{type}
\begin{code}
detResidueGC c myMap ps x 
    = let dy = trace "Calling chunking Gauss..." $ 
               gaussResidueGC c myMap ps x -- returns full matrix
          d =  map unmaybe $ diag dy
          res = product d
      in res
\end{code}
\begin{codex}
detResidueGCD c myMap ps x 
    = let dy = trace "Calling diagonal Gauss..." $ 
               gaussResidueGCD c myMap ps x -- returns diagonals
               d1 = map unmaybe $ dy
               res = rnf d `seq` product d
      in res
\end{codex}
\begin{type}
detResidueGCD :: (Trans a, Trans b, Trans n, Trans x, Num n, Num x) =>
                 Int -> Map a b -> [n] -> MatArr Int x -> x
\end{type}
\begin{code}
detResidueGCD c myMap ps x 
    = let ys = trace "Calling Gauss..." $ 
               gaussResidueGCD c myMap ps x
          ds = map unmaybe ys
          res = rnf ds `seq` 
                product ds
      in res
detResidueGCL c myMap ps x 
    = let ys = trace "Calling Gauss..." $ 
               gaussResidueGCL c myMap ps x
          ds = map unmaybe ys
          res = rnf ds `seq` 
                product ds
      in res
detResidueGCD2 c myMap ps x 
    = let ys = trace "Calling Gauss..." $ 
               gaussResidueGCD2 c myMap ps x
          ds = map unmaybe ys
          res = rnf ds `seq` 
                product ds
      in res

\end{code}                      