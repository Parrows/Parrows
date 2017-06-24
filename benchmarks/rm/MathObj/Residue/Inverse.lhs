Erweiterter euklidischer Algorithmus.
Die Version hier findet Inversen modulo n.
\begin{code}
module MathObj.Residue.Inverse where

import Prelude
import MathObj.Residue.Euclid
\end{code}

Nun fuer eea ((p, a), (0, 1)) und p prim ist das Ergebnis ((1, 0),(b, -p)). Dabei ist b==a^-1 mod p.
Ist in ((x, _),(_,_)) nicht 1, dann war p nicht prim und es gibt keine Inversen fuer a mod p.

\begin{code}
inverse a p =  let ((x, _), (b, _)) = eea ((p, a), (0, 1))
               in if (x/=1) then error "No inverse found!"
                  else b
\end{code}

