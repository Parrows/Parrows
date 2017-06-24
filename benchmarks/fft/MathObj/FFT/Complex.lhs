Alles ueber komplexe Zahlen, Einheitswurzeln, etc.

\begin{code}
module MathObj.FFT.Complex where

import Data.Complex
import MathObj.Vector.Vector
\end{code}

In die komplexe Datenstruktur und zurueck.
\begin{code}
lift :: (Num a, Integral a, RealFloat b) => [a] -> [Complex b]
lift = map (fromIntegral)

showC :: (RealFloat a, Integral b) => [Complex a] -> [b]
showC = map (round . realPart)

unlift = showC
\end{code}

Einheitswurzel. TODO: Umformulieren mit arbitrary precision!
\begin{code}
-- possible limitation due to length of mantissa in realfloat
-- root_of_unity :: (RealFloat a, Integral b) => b -> b -> Complex a
root_of_unity j n = cos (2*pi*j'/n') :+ sin (2*pi*j'/n')
    where j' = fromIntegral j
          n' = fromIntegral n
\end{code}

Listen von Einheitswurzeln. Egal, was momentan die Liste ist! TODO: Typ aendern, falls Liste anders aussieht!
\begin{code}
powers :: RealFloat a => Int -> Complex a -> (Int, [Complex a])
powers n w = fromList $ take n $ iterate (*w) 1
\end{code}