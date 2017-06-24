Variationen ueber den euklidischen Algorithmus
\begin{code}
module MathObj.Residue.Euclid where

import Prelude
import Debug.Trace
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


3-spaltiger EEA
\begin{code}
eeaStep3 [(a1, a2), (b1, b2), (c1, c2)] = [(a2, a3), (b2, b3), (c2, c3)]
    where (t, a3) = divMod a1 a2
          b3 = b1 - t*b2
          c3 = c1 - t*c2
eeaStep3 _ = error "eeaStep3"

eea3 [(a1, a2), (b1, b2), (c1, c2)]  | a2/=0 = eea3 $ eeaStep3 [(a1, a2), (b1, b2), (c1, c2)]
                                     | otherwise = [(a1, a2), (b1, b2), (c1, c2)]
\end{code}

EEA mit Suche
\begin{code}
-- criteria x y n = trace ("criteria: " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ show n ++ " --> " ++ (show $ abs(x)<n && abs(y)<n)) $ abs(x)<n && abs(y)<n
criteria x y n = abs(x)<n && abs(y)<n
-- eeaSearch :: (Integral i, Integral j) => ((i, j), (i, i)) -> i -> Maybe (i, i)
eeaSearch ((a1, a2), (b1, b2)) n | a2==0 = Nothing
                                 | a2/=0 && not (criteria a2 b2 n)
                                     = flip eeaSearch n $ eeaStep ((a1, a2), (b1, b2))
                                     -- = trace ( "eeaSearch: " ++ (show ((a1, a2), (b1, b2))))
                                     -- flip eeaSearch n $ eeaStep ((a1, a2), (b1, b2))
                                 | otherwise = Just (a2, b2)
\end{code}

Verbose
\begin{code}
eeaSearchV ((a1, a2), (b1, b2)) n 
    | a2==0 = Nothing
    | a2/=0 && not (criteria a2 b2 n)
        = trace ( "eeaSearch: " ++ (show ((a1, a2), (b1, b2)))) $
          flip eeaSearchV n $ eeaStep ((a1, a2), (b1, b2))
    | otherwise 
        = trace ( "eeaSearch: criteria matches: " 
                  ++ (show ((a1, a2), (b1, b2)))
                  ++ " for n = " ++ show n ) $
          Just (a2, b2)

restoreFraction a m n = eeaSearchV ((a, m), (0, 1)) n
\end{code}