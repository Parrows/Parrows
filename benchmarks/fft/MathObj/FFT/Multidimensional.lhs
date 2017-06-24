Multidimensionales FFT.

Fuer 2D wir operieren erstmal auf Spalten und dann auf Zeilen. Die Vorgehensweise kann verallgemeinert werden.
\begin{code}
module MathObj.FFT.Multidimensional where

import MathObj.FFT.Short (fftS) -- oder anderes 1D FFT unserer Wahl
import MathObj.Vector.Vector hiding (transpose, length)
import qualified Prelude -- hide!
import Prelude hiding ((++), concat, length)
import Data.List (transpose)

import MathObj.Primes.SimpleFactor (factor)
import MathObj.FFT.IndexTransform
\end{code}

2D
\begin{code}
twoDimFft n1 n2 xss = map (fft' n2) $ transpose $ map (fft' n1) $ transpose xss
\end{code}

\begin{code}
fft' n xs = toList $ fftS (n, xs)
\end{code}

Fuer n-dim. FFT brauchen wir eine Art geschachtelte Listen zu "transponieren", in array-termen: x[i,j,k,...] nicht nach i, sondern nach j, k,... durchzuiterieren.


------------------------------------

Misplaced? Move to multidim/prime factor/.... fft?

Hatten wir's nicht schon irgendwo?
\begin{nocode}
splitFactors (n, xs) = let ns = factor n
                       in splitter ns xs

splitter [] _ = []
splitter (k:ks) xs = (take k xs):splitter ks xs

splitFactor2 n1 n2 (n, xs) | n1*n2 /= n = error "Factorisation mismatch!"
                           | otherwise  = splitter ks xs
                           where ks = take n2 $ repeat n1
\end{nocode}
\begin{nocode}
twoDimFft xss = let fftList = toList $ fft $ fromList  
                    yss = map (fftList ) xss
                in map (fftList) $ transpose yss
                           
factorFftRaw n1 n2 (n, xs) =  let xss = splitFactor2 n1 n2 (n, xs)
                              in concat $ twoDimFft xss

getFactors n = let n2 = tail $ factor n
                   n1 = n `div` n2
               in (n1, n2)

factorFft (n, xs) = applyTransform (factorFftRaw n1 n2) n1 n2 (n, xs)
    where (n1, n2) = getFactors n
\end{nocode}