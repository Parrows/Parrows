Erweiterter euklidischer Algorithmus.
Die Version hier findet Inversen modulo n.
\begin{code}
module MathObj.Residue.Inverse where

import Prelude
\end{code}

a1 b1
a2 b2
-----
a3 b3

\begin{code}
eeaStep ((a1, a2), (b1, b2)) = ((a2, a3), (b2, b3))
    where (t, a3) = divMod a1 a2
          b3 = b1 - t*b2

eea ((a1, a2), (b1, b2)) | a2/=0 = eea $ eeaStep ((a1, a2), (b1, b2))
                         | otherwise = ((a1, a2), (b1, b2))
\end{code}

Nun fuer eea ((p, a), (0, 1)) und p prim ist das Ergebnis ((1, 0),(b, -p)). Dabei ist b==a^-1 mod p.
Ist in ((x, _),(_,_)) nicht 1, dann war p nicht prim und es gibt keine Inversen fuer a mod p.

\begin{code}
inverse a p =  let ((x, _), (b, _)) = eea ((p, a), (0, 1))
               in if (x/=1) then error "No inverse found!"
                  else b
\end{code}