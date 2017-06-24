
Mit dieser Datei schalten wir die Vektor-Implementierungen um.

\begin{code}
{-# OPTIONS -fno-monomorphism-restriction #-}
module MathObj.Vector.Vector (length, concat, transpose, (@+), (@-), (@*), (@/), ($/), ($*), (!!), splitAt, filter, fromList, toList, (++))
    where
\end{code}

Die Moeglichkleiten sind:

* "normale" Listen
* Listen mit Laenge
* chunked Listen mit Laenge (TODO)

ggf. werden Hinzugefuegt:

* Arrays

Die Implementierung wird hier included. Diese Zeile wird geaendert, wen wir auf die andere Implementierung schalten!

\begin{code}
import qualified Prelude -- hide standard implementations
import MathObj.Vector.Indexed 
    -- (length, concat, transpose, add, substr, mult, divide, divideSkalar, multSkalar, (!!), splitAt, filter, fromList, toList, (++))
\end{code}

Alle diese Strukturen muessen die Funktionen an der rechten Seite implementieren:

\begin{code}
(@+) = add
(@-) = substr
(@*) = mult
(@/) = divide
($/) = divideSkalar
($*) = multSkalar
\end{code}
