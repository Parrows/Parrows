Trivialler Skelett, fuer sequentielle DC Algorithmen.

\begin{code}
module Parallel.Skel.Trivial where
\end{code}

dc isTrivial divide trivial merge

\begin{code}
dc :: (a -> Bool) -> (a -> [a]) -> (a -> b) -> ([b] -> b) -> a -> b
dc isTrivial divide trivial merge args
   | isTrivial args = trivial args
   | otherwise = 
       (merge . map(self) . divide) args
           where self = dc isTrivial divide trivial merge
\end{code}