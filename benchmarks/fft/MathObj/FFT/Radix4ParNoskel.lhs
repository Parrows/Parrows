4-radix FFT, parallel. Kein Skelett.

\begin{code}
module MathObj.FFT.Radix4ParNoskel where

import Parallel.Skel.Trivial (dc)
import MathObj.Vector.Vector hiding ((!!))
import MathObj.Vector.Partitions
import MathObj.FFT.Complex
-- import qualified MathObj.FFT.Radix2TimeSeq as R2 (fft)
import MathObj.FFT.Short (fftS)
import qualified Prelude -- hide, hide, hide
import Prelude ((<), error, (!!), (*), ($), RealFloat, (>), (<), otherwise, zip, unzip, curry, uncurry, map)
import Data.Complex

import Parallel.Skel.Quad (parmap)
import Parallel.Eden hiding (merge)
import System.IO.Unsafe (unsafePerformIO) -- use new interface!

import MathObj.Vector.Chunked

instance (Trans a, RealFloat a) => Trans (Complex a)
\end{code}

Nussbaumer, s.92
\begin{code}
fftDo k p xs  -- = dc isTrivial (splitR 4) fftS merge xs
         | isTrivial xs = fftS xs
         | otherwise = merge k p $ map (fftDo k p) $ splitR 4 xs
    where isTrivial xs = length xs < 4
\end{code}
\begin{nocode}          
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
\end{nocode}

Wir geben daie Task ab, falls die Laenge stimmt.
TODO: doch treshold?
\begin{nocode}
spawn2 f x y | length x > parLength -- = unsafePerformIO $ instantiate (process f) $ x y
                 = deLift (createProcess (process f) x y)
             | otherwise = f x y
\end{nocode}
\begin{code}
-- dirty trick!
spawn2 chunkLength parLength f x y | length x > parLength
                                       = let f' = cLift2 chunkLength f
                                             x' = chunk chunkLength x
                                             y' = chunk chunkLength y
                                         in unchunk $ deLift $ createProcess (process (f' x')) y'
                                   | otherwise = f x y
-- spawn2i x f y = spawn2 f x y
\end{code}
\begin{code}
merge k p xss = r0 ++ r1 ++ r2 ++ r3
    where xs0 = xss!!0
          xs1 = xss!!1 @* powers n w
          xs2 = spawn2 k p (@*) (xss!!2) (powers n (w*w))
          xs3 = spawn2 k p (@*) (xss!!3)  (powers n (w*w*w))
          n = length xs0
          w = root_of_unity (-1) (4*n)
          oddsPlus = spawn2 k p (@+) xs0 xs2
          evensPlus = spawn2 k p (@+) xs1 xs3
          oddsMinus = spawn2 k p (@-) xs0 xs2
          -- twiddle = spawn2 k p ($*) (xs1 @- xs3) (0:+1) -- lokal wird `xs1 @- xs3` ausgewertet
          twiddle = (spawn2 k p (@-) xs1 xs3) $* (0:+1) -- wegen typ von spawn2
          r0 = spawn2 k p (@+) oddsPlus evensPlus
          r1 = spawn2 k p (@-) oddsMinus twiddle
          r2 = spawn2 k p (@-) oddsPlus evensPlus
          r3 = oddsMinus @+ twiddle
\end{code}
Schnittstelle:
\begin{code}
fft = fftDo
\end{code}
ifft??
