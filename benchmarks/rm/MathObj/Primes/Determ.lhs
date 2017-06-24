A fast, but ugly method of generating primes.

This is -*- Literate Haskell -*-

No number theory behind it, just brute force! But deterministic.

Stolen from LinSolv.
\begin{code}
module MathObj.Primes.Determ where

import Debug.Trace (trace)

primes :: [Integer]
primes = 2 : oddPrimesFrom 3
         where oddPrimesFrom n | isPrime n primes = n : oddPrimesFrom (n+2)
                               | otherwise        =     oddPrimesFrom (n+2)

isPrime :: Integer -> [Integer] -> Bool
isPrime n (l1:ls) | n < l1*l1        = True
                  | n `mod` l1 == 0  = False
                  | otherwise        = isPrime n ls

\end{code}

Primes from some Limit
\begin{code}
primesFrom l = trace "Generating primes..." $ dropWhile (<l) primes
\end{code}