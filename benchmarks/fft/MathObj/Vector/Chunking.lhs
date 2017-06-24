Altes chunking Code, umgebaut.

\begin{code}
module MathObj.Vector.Chunking where 

import qualified Data.List as DL (concat, splitAt)
\end{code}
\begin{code}
-- fftchunk :: (Trans a, RealFloat a) => Int -> FFT a -> (Int, [[Complex a]])
chunk size (n,cs) = (n, chunk' size cs)

-- unchunk :: (Trans a, RealFloat a) =>  (Int, [[Complex a]]) -> FFT a
unchunk  (n,css) = (n, DL.concat css)

chunk'      :: Int -> [a] -> [[a]]
chunk' _ [] =  []
chunk' n xs =  ys : chunk' n zs
    where (ys,zs) = DL.splitAt n xs

unchunk' :: [[a]] -> [a]
unchunk' = DL.concat
\end{code}
