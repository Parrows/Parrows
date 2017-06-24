Die Indextransformationen von Nussbaumer, s. 125f.

\begin{code}
{-# OPTIONS -XParallelListComp #-}
module MathObj.FFT.IndexTransform where

import Prelude
import MathObj.Residue.Inverse (inverse)
import Data.List
\end{code}

Die Laengen:
l=l1*l2
ggt(l1, l2)==1

Die Indizes der eindimensionalen Transformationen:
n1, n2

Indextranfsformation:
n = l1*n2 + n2*n1 mod l, n1,k1=[0..l1-1]
k = l1*k2 + l2*k1 mod l, n2,k2=[0..l2-1]

Dabei ist es besser k anders zu berechnen:

t1 = n2^{-1} mod n1
t2 = n1^{-1} mod n2

Das gibt ueberigens die Permunation auf den Indizes der 1-dim. Transformation an. Somit ist

k = l1*t1*k2 + l2*t2*k1 mod l

----------------------------

indexTransform bekommt zwei Teilerfremde Faktoren n1 und n2 nach prime factor Verfahren. Wir liefern die permutierten Listen der Indizes (is, js). Dann kann man die eigentliche Eingabe auf n1 Teillisten der Laenge n2 zerhacken -- sei xss das Ergebnis -- und fuer deren Eintraege permutieren: 

[ xss!!n!!k, n<-is!!i, k<-js!!j, i<-[1..n1], j<-[1..n2] ]

TODO: zipN und nur ein mal listen durchlaufen?
\begin{code}
indexTransform n1 n2 = ([(n1*j    + n2*i   ) `mod` n | i<-[0..n1-1], j<-[0..n2-1] ],
                        [(n1*j*t1 + n2*i*t2) `mod` n | i<-[0..n1-1], j<-[0..n2-1] ])
    where n  = n1*n2
          t1 = inverse n2 n1
          t2 = inverse n1 n2
\end{code}

Und so wird's angewanndt:
\begin{nocode}
applyTransform n1 n2 xss (is, js) = [ xss!!n!!k | n<-(is!!i), k<-(js!!j), let i<-[1..n1], let j<-[1..n2] ]
\end{nocode}

\begin{code}
-- applyTransformDo' xs is = [xs!!i | (i+1)<-is]
--- man braucht sowieso alle elemente hier!
applyTransformDo' xs is | (length xs - 1) == length is && not (elem 0 is)
                            -- wir koennen das umgehen
                            = head xs : applyTransformDo' (tail xs) is
                        | length xs < length is = error ("Permutation to long! " ++ (show $ length xs) ++ "/=" ++ (show $ length is) )
                        | length xs > length is = error "Permutation to short!"
--                            = [ if j `elem` is then xs!!i else xs!!j | (j+1)<-[1..length xs] 
--                                                                     | (i+1)<-is ]
                        | otherwise {- same length -} = [xs!!i | (i+1)<-is]

applyTransform' (f) n1 n2 xs = let (is, js) = indexTransform n1 n2
                               in flip applyTransformDo' js $ f $ applyTransformDo' xs is

-- applyTransformDo (n, xs) is | length is /= n = error "IndexTransform: Permutation to short!"
--                            | otherwise      = (n, applyTransformDo' xs is)
applyTransformDo (n, xs) is = (n, applyTransformDo' xs is)
applyTransform f n1 n2 (n, xs) = let (is, js) = indexTransform n1 n2
                                 in flip applyTransformDo js $ f $ applyTransformDo (n, xs) is
\end{code}
TODO: generisch umschreiben, sodass es fuer alle listentypen (indexed, unindexed, ...) passt.

----------------------------------------------------------------

indexTransformW berechnet die Indextransformationen nach Winograd. n1 und n2 muessen nicht unbedingt koprim sein.

Eingabepermutation: a_{r1j1n1+r2j2n2 mod n} -> b_{j1n1+j2}
Ausgabepermutation: A_{s1k1n1+s2k2n2 mod n} -> B_{k1n1+k2}

Eine einfache Wahl fuer r1, r2, s1, s2 ist r1=r2=1 und s1, s2 aus CRT. Es muss gelten:

r1s1n1=1 mod n2
r2s2n2=1 mod n1

--------------

Eingabepermutation vereinfacht:
a_{j1n1+j2n2 mod n} -> b_{j1n1+j2} // seq.
Ausgabepermutation vereinfacht:
A_{s1k1n1+s2k2n2 mod n} -> B_{k1n1+k2} // seq, nichts zum vereinfachen!
\begin{nocode}
indexTransformW n1 n2 = let s1 = inverse n1 n2
                            s2 = inverse n2 n1
                            n  = n1*n2
                            inPerm = [(j1*n1 + j2*n2) `mod` n | j1<-[0..n1-1], j2=[0..n2-1] ]
                            outPerm = [(s1*k1*n1 + s2*k2*n2) `mod` n | k1<-[0..n1-1], k2=[0..n2-1] ]
                        in (inPerm, outPerm)
\end{nocode}

ES IST DAS SELBE WIE IN NUSSBAUMER!!!


----------------------------------------------------------------------------------
