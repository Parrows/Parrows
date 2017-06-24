Rader-Brenner FFT. Nussbaumer, s. 99f. Das ist die sequentielle Variante.

Wir reduzieren Anzahl der komplexwertigen Multiplikationen durch geschickte Substitutionen.
\begin{code}
module MathObj.FFT.RadixBrennerSeq where

import MathObj.FFT.Radix2TimeSeq (fft)
import MathObj.Vector.Vector hiding ((!!), (++))
import MathObj.Vector.Partitions
import Parallel.Skel.Trivial (dc)
import Prelude hiding (length)
import MathObj.FFT.Complex
import Data.Complex
\end{code}

Das ist Radix-2 DIT.
Wir verwenden den trivialen DC Skelett.

\begin{nocode}
fftDo (n, xs) kind = dc isTrivial divide id (merge kind) (n, xs)
    where isTrivial (n, _) = n==1
          divide xs = [mod ls, mod rs]
              where [ls, rs] = splitOddEven xs
                    mod xs = xs @* powers (length xs) (root_of_unity 1 (length xs))
\end{nocode}

Wir definieren die Liste $a_m$ der Laenge $n/2$ ausgehend von der Liste der ungeraden Eintraegen fuer $n$-DFT "in time".

`xs` ist die "ungerade Liste", `ausSeq xs` wird zu $a_m$.
\begin{code}
toAuxSeq :: (Int, [Complex Double]) -> [Complex Double]
toAuxSeq (n, ys) = 0:[formula i | i<-[1..n`div`2-1] ] ++ 0:[formula i | i<-[n`div`2+1..n] ]
    where formula i = (ys!!i-ys!!(i+n))/((2.0*cos(pi*i'/n')):+0)
              where n' = fromIntegral n
                    i' = fromIntegral i
\end{code}

Nun muessen wir zurueck zu transformierten $\hat{x}_{2k+1}$ nachdem wir $a_m$ n/2-DFT transformiert haben.

Eingabe: as -- transformierte toAusSeq von ungeraden Liste, xs -- 0, n/4, n/2, 3n/4 Elemente der originalenListe, der *nicht*-fouriertransformierten Eingabe von fft der Laenge n.
\begin{code}
fromAuxSeq (n, as) (4, xs) = fromList [as!!i + as!!(i+1) + v i | i<-[0..n-1] ]
    where v i | even i = xs!!0 - xs!!2 - img
              | odd i  = xs!!0 - xs!!2 + img
              where img = (xs!!1 - xs!!3)*(0:+1)
fromAuxSeq _ _ = error "List matching error!"
\end{code}


Jetzt zu dem modifizierten DIT FFT. Wir verwenden den trivialen DC Skelett.

\begin{code}
fftDo (n, xs) kind = dc isTrivial divide id (merge kind (n, xs)) (n, xs)
    where isTrivial (n, _) = n==1
          divide xs = [mod ls, mod as]
              where [ls, rs] = splitOddEven xs
                    mod xs = xs @* powers (length xs) (root_of_unity 1 (length xs))
                    as = toAuxSeq rs
                    
merge kind (k, ps) [(n, xs), (_, ys)] = (2*n, l1 ++ l2)
    where (l1, l2) = unzip [(e+o', e-o') | (e,o,r) <- zip3 xs rs unitRoots, let o'=o*r]
          (_, unitRoots) = powers n (root_of_unity kind n)
          rs = fromAuxSeq ys $ fromList ps!!0:ps!!(k`div`4 - 1):ps!!(k`div`2 - 1):ps!!(3*k`div`4 - 1):ps!!(k - 1)
merge _ _ _ = error "List content mismatch!"
\end{code}
