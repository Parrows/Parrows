Potenzierungsalgorithmen modulo n.
\begin{code}
module MathObj.Residue.Power {-(powM, normalize)-} where

import MathObj.Primes.SimplePrimeTest (isPrime)
import Prelude 
\end{code}

\begin{code}
powM = powMF
\end{code}

Das ist ein Ueberbleibsel der alten Version. Wird spaeter irgendwo bei Restklassen verwendet. Es brint
 die halbwegs symmetrischen Reste von `mod` zu der traditionellen Version.
\begin{nocode}
normalize a p | a>=0 && a<p = a
              | a>=0 && a>p = a `mod` p
              | a<0         = (p + (a `mod` p)) `mod` p
\end{nocode}

Wir brauchen hier die Typisierung, ansonsten modulo 2^n beim Aufruf aus powMF!
\begin{code}
powMNaive g u p = let k = g^u :: Integer
                  in k `mod` p
\end{code}

\begin{nocode}
--debug:
powMNaive' g u p = let msg = "Gone naive at u=" ++ show u
                   in error msg
\end{nocode}

Fermats keiner Satz:

a^{p-1} = 1 mod p

Vorversion, fuer echten Code blaettere weiter.
\begin{nocode}
powMF g u p | not (isPrime p)  = powMNaive g u p
            | u<p = powMNaive g u p
--            | otherwise                = powMF g (u-p+1) p
--              | u<2*p = powMF g (u-p) p
              | otherwise -- u is sehr gross, komplizierte Rechnung loht sich!
                  = powMF g (u `mod` (p-1)) p
\end{nocode}

Wir bauen eine Worker-Funktion ein, um nur ein mal zu pruefen, ob p prim ist.
Ausserdem haben wir fuer relativ kleiene u eine andere Routine: *2, Addition und ein Vergleich mehr ist billiger als eine Division. 

fromIntegral nicht ausbauen, ansonsten: Probleme mit Int vs. Integer. Diese Unterscheidung kommt auch zustande weil mit Int bekommen wir unsere Ergebnisse modulo 2^n: 3^27 :: Int /= 3^27 :: Integer.

\begin{code}
-- powMF :: Integral a => a -> a -> a -> a
powMF g u p | u<p || not (isPrime p) = fromIntegral $ powMNaive g u p
            | otherwise = powMF' g u p

powMF' g u p | u<p   = fromIntegral $ powMNaive g u p
             | u<2*p = powMF' g (u-p+1) p
             | otherwise -- u is sehr gross, komplizierte Rechnung loht sich!
                = powMF' g (u `mod` (p-1)) p
\end{code}
