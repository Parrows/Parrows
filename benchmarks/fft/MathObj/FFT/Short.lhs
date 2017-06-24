Die *kurzen* FFTs, Nussbaumer, s.145ff.

\begin{code}
module MathObj.FFT.Short where

import Data.Complex

fftS (0, []) = error "Empty input!"
fftS (1, [x0]) = (1, [x0])
fftS (2, [x0, x1]) = (2, [x0+x1, x0-x1])
fftS (3, [x0, x1, x2]) = let i = 0.0 :+ 1.0
                             u = 2.0*pi/3.0
                             t1 = x1+x2
                             m0 = x0+t1
                             m1 = (cos(u) - 1.0) * t1
                             m2 = i * sin(u) * (x2-x1)
                             s1 = m0 + m1
                         in (3, [m0, s1+m2, s1-m2])
fftS (4, [x0, x1, x2, x3]) = let i = 0.0 :+ 1.0
                                 t1 = x0 + x2
                                 t2 = x1 + x3
                                 m0 = t1 + t2
                                 m1 = t1 - t2
                                 m2 = x0 - x2
                                 m3 = (x3 - x1) * i
                             in (4, [m0, m2+m3, m1, m2-m3])
\end{code}

Die naechsten sind noch Laenger und brauchen polynomielle Reste.

TODO

... und als allerletztes:
\begin{code}
fftS _ = error "Input length not known!"
\end{code}