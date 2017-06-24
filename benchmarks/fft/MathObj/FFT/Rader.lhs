Der Rader Algorithmus, Nussbaumer, s.117ff., s.125ff.

\begin{code}
module MathObj.FFT.Rader where

import MathObj.Vector.Vector (fromList, toList)
import MathObj.FFT.Convolution
import MathObj.Primes.SimpleFactor
import MathObj.FFT.Complex
import MathObj.Residue.RootOfUnity
import MathObj.Residue.Power
import qualified Prelude as P
import Prelude ((!!), ($), (+), (-), (*), (/), head, tail, take, iterate, filter, not, any, sum, id, Int)
\end{code}

--------------------------------------------------------
Die Indextransformation *ist*  ein Potenzierungsalgorithmus.

TODO:
- besser?
- move in Residue/
\begin{nocode}
powM g 0 p = g `mod` p
powM g u p = pow (g*g `mod` p) (u-1) p
\end{nocode}
Technisch:
\begin{code}
powMI :: Int -> Int -> Int
powMI = P.fromIntegral $ powM
\end{code}
g ist Ein Einheitswurzel modulo p!


Nun die eigentliche Liste 'rumschieben!
\begin{code}
trans :: Int -> (Int -> Int) -> [a] -> Int -> [a] 
trans g f xs p = [xs!!(powMI g (f i) p) | i<-[0..P.length xs-1] ]

transVon g xs p = trans g id xs p
transNach g xs p = trans g (\x -> -x) xs p
\end{code}
--------------------------------------------------------

Die Faltung.

TODO: nach Convolution.lhs verschieben?
ggf. alten code aus polymult borgen!

\begin{nocode}
conv f g = ifft $ (@*) $ fft f $ fft g
\end{nocode}

--------------------------------------------------------

hat{x}_0 ist bloss die Summe

\begin{code}
x0 xs = sum xs
\end{code}

--------------------------------------------------------

Rader Algorithmus
 TODO: alles in Rader.lhs und Rest spaeter in Primfaktorzerlegung.lhs und hier nur Aufruf von Zerlegung + Rader fuer Faktoren?

Auf jeden Fall, hier ist der Algorithmus fuer prime Laengen, n ist eine Primzahl!

\begin{code}
rader (n, x:xs) = 
    let ys = fromList $ transNach g xs n
        zs = fromList $ transVon g (take n $ iterate (*w) 1) n
        rests = transNach g f n
        f = toList $ conv ys zs
        w = root_of_unity 1 n
        g = rootOfUnityModulo n
    in (n, (x0 (x:xs)) : rests)
\end{code}