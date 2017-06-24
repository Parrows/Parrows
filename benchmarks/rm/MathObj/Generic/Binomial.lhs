Binomialkoeffizienten

\begin{code}
module MathObj.Generic.Binomial where
\end{code}

Fakultaet.
\begin{code}
fac n = product [1..n]
\end{code}

Binomialkoeffizient n`over`k = n!/(k!(n-k)!)
\begin{code}
binomial n k = (fac n)`div`((fac k)*(fac $ n-k))
n `over` k = binomial n k
\end{code}