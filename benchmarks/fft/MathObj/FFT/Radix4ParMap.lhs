4-radix FFT, parallel

\begin{code}
module MathObj.FFT.Radix4Par where

import Parallel.Skel.Trivial (dc)
import MathObj.Vector.Vector hiding ((!!))
import MathObj.Vector.Partitions
import MathObj.FFT.Complex
-- import qualified MathObj.FFT.Radix2TimeSeq as R2 (fft)
import MathObj.FFT.Short (fftS)
import qualified Prelude -- hide, hide, hide
import Prelude ((<), error, (!!), (*), ($), RealFloat)
import Data.Complex

import Parallel.Skel.Quad (parmap)
import Parallel.Eden (noPe, Trans)

instance (Trans a, RealFloat a) => Trans (Complex a)
\end{code}

Nussbaumer, s.92
\begin{code}
fftDo xs = dc noPe isTrivial fftS (splitR 4) merge xs
    where isTrivial xs = length xs < 4
          
merge xss = r0 ++ r1 ++ r2 ++ r3
-- merge xss = r0 ++ r3 ++ r2 ++ r1
    where xs0 = xss!!0
          xs1 = xss!!1 @* powers n w
          xs2 = xss!!2 @* powers n (w*w)
          xs3 = xss!!3 @* powers n (w*w*w)
          n = length xs0
          w = root_of_unity (-1) (4*n)
          oddsPlus = xs0 @+ xs2
          evensPlus = xs1 @+ xs3
          oddsMinus = xs0 @- xs2
          twiddle = (xs1 @- xs3) $* (0:+1)
          r0 = oddsPlus @+ evensPlus
          r1 = oddsMinus @- twiddle
          r2 = oddsPlus @- evensPlus
          r3 = oddsMinus @+ twiddle
--          r3 = (oddsMinus @+ twiddle) $* (-1)

-- merge _ = error "Wrong input list!"
\end{code}

Schnittstelle:
\begin{code}
fft = fftDo
\end{code}
ifft??
