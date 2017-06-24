Cooly-Tuckey Decimation in Frequency FFT. Sequentiell.

\begin{code}
module MathObj.FFT.Radix2FreqSeq (fft, ifft) where

import Parallel.Skel.Trivial (dc)
import MathObj.Vector.Vector
import MathObj.Vector.Partitions
import MathObj.FFT.Complex
import qualified Prelude -- hide standard implementations
import Prelude ((.), id, (<=), (==), ($), (*), Int, Num, RealFloat, map)
import Data.Complex (Complex)
\end{code}

Wir verwenden hier den trivialen DC Skelett.

Beides fft und ifft sind formuliert in Termen von fftDo.

\begin{code}
fftDo :: RealFloat r => (Int, [Complex r], Complex r) -> (Int, [Complex r])
fftDo (n, xs, w) = dc isTrivial divide trivial combine (n, xs, w)
    where divide (n, xs, w) = [(k, ls, ww), (k, mods, ww)]
              where ((_, ls), (k, rs)) = splitHalves (n, xs)
                    (_, mods) = (k, rs) @* (powers k ww)
                    ww = w*w
          isTrivial (n, _, _) = n==1
          trivial (n, xs, _) = (n, xs)
          combine = concat . transpose
\end{code}

Nun die eigentliche Schnittstelle:
\begin{code}
-- it fails with typedesc
-- fft, ifft :: RealFloat r => (Int, [Complex r]) -> (Int, [Complex r])
fft (n, xs) = fftDo (n, xs, root_of_unity 1 n)

ifft (n, xs) = fftDo (n, xs, root_of_unity (-1) n)
\end{code}