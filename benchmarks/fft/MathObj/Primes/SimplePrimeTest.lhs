The Dark Ages of prmality testing. Still good enough for small values.

\begin{code}
module MathObj.Primes.SimplePrimeTest where

import MathObj.Primes.Sieve (primes)
\end{code}

We look through the primes and check if it fits.
\begin{code}
isPrime n | even n && n>2 = False
          | n < 2         = False
          | otherwise     
--              = (<) 0 $ length $ dropWhile (< n) $ takeWhile (<= n) primes
                = elem n $ takeWhile (<= n) primes
\end{code}