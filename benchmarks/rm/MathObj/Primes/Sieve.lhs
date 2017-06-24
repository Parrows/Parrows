Das Sieb des Eratosthenes.

\begin{code}
module MathObj.Primes.Sieve where
\end{code}

Erstmal das Sieb. Wir bedienen uns der Implementierung von \url{http://okmij.org/ftp/Haskell/number-sieve.lhs}. Zitiere:
\begin{quote}The algorithm only relies on the successor, predecessor and zero
comparison. The predecessor can be easily eliminated. Thus the
algorithm can be used with Church and Peano numerals, or members of
Elliptic rings, where zero comparison and successor take constant
time but other arithmetic operations are more involved.\end{quote}

\begin{code}
-- repl_every_n n l replaces every (n+1)-th element in a list (_:l)
-- with False
repl_every_n :: Integral a => a -> [Bool] -> [Bool]
repl_every_n n l = repl_every_n' n l
    where repl_every_n' 0 (_:t) = False: repl_every_n n t
          repl_every_n' i (h:t) = h:     repl_every_n' (pred i) t

primes = 2:(loop 3 (repeat True))
    where loop n (False:t) = loop (succ (succ n)) t
          loop n (_:t)  = n:(loop (succ (succ n)) (repl_every_n (pred n) t))
\end{code}
