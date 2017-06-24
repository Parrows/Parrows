FFT mit Primfaktorzerlegung. Sequentiell.
\begin{code}
module MathObj.FFT.PrimeFactor.Seq (fft) where
\end{code}

imports:
\begin{code}
import MathObj.Vector.Vector
import MathObj.Primes.SimpleFactor (factors)
\end{code}

Unterscheidung von versch. Algorithmen
\begin{code}
import MathObj.FFT.Reference as R2 (fft) -- we currently misuse the reference alg. for R2 one
import MathObj.FFT.Rader as Rader (fft)  -- prime length FFT
\end{code}

\begin{code}
splitF xs = let r = head $ factors $ length xs
            in splitN r xs
\end{code}

Hybrid algorithm.

* add a test for R4?
* the call in Convolution.lhs should refer to this!
\begin{code}
fft xs | isPower2 $ length xs = R2.fft xs
       | isPrime  $ legnth xs = Rader.fft xs
       | otherwise            = fftDoPrimeFactor xs
\end{code}

The actual work iff `length xs` is a composite number and not a power of two.

WRONG! We have to mix right!
use 2D FFT for it!!!
\begin{nocode}
fftDoPrimeFactor xs = let splits = splitF xs
                      in map (fft) splits
                         
\end{nocode}