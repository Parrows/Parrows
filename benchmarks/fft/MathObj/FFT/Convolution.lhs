Die Faltung.
\begin{code}
module MathObj.FFT.Convolution where

import MathObj.FFT.Reference (fft, ifft)
import MathObj.Vector.Vector
\end{code}

ggf. altes code aus polymult borgen!

TODO: richtiges und korrektes Algorithmus verwenden!

\begin{code}
conv f g = ifft $ (fft f) @* (fft g)
\end{code}
