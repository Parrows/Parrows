Alte Skeletons.
\begin{code}
module Parallel.Skel.Binary where

import Parallel.Eden
\end{code}

with additional depth control parameter:
\begin{code}
bindivConD :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
bindivConD numProc trivial solve split combine x = 
      if numProc <= 0 then seqDC x
                       else if trivial x then solve x 
                       else child `seq`  myself1 `seq` combine [myself1, deLift child] 
                            -- (parMap myself (split x)))
	    where myself = bindivConD (div numProc 2) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r]  = split x
	          child  = createProcess (process myself)  r
	          myself1 = myself l
\end{code}
---- with stream chunking
--binDCStr:: (Trans a, Trans b) => 
--           Int -> Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
--binDCStr numProc chunkSize trivial solve split combine x = 
--      if numProc <= 0 then seqDC x
--                       else if trivial x then solve x 
--                       else child `seq` rnf myself1 `seq` combine [myself1, deLift child] 
--                            -- (parMap myself (split x)))
--	    where myself = bindivConD (div numProc 2) trivial solve split combine
--	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
--	          [l,r]  = split x
--	          child  = createProcess (process myself)  r
--	          myself1 = myself l
--	          

with additional depth control parameter and explicit placement
\begin{code}
bindivConDP :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
bindivConDP n trivial solve split combine x 
    -- = bindivConDAtdm n 0 trivial solve split combine x ()
    = bindivConDAt n 0 trivial solve split combine x

bindivConDAt :: (Trans a, Trans b) => 
           Int -> Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
bindivConDAt numProc depth trivial solve split combine x = 
      if numProc <= 1 then seqDC x
                       else if trivial x then solve x 
                       else child `seq`  
		            rnf myself1 `seq` 
			    combine [myself1, deLift child] 
                            -- (parMap myself (split x)))
	    where myself = bindivConDAt (div numProc 2) (depth +1) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r]  = split x
	          child  = cpAt (selfPe + shift) (process (myself)) r
	          shift  = 2 ^ depth
	          myself1 = myself l
\end{code}

direct-mapping version, exchange inside bindivConDP if needed
\begin{code}
bindivConDAtdm :: (Trans a, Trans b) => 
           Int -> Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> () ->  b
bindivConDAtdm numProc depth trivial solve split combine x = \ _ ->
      if numProc <= 1 then seqDC x
                       else if trivial x then solve x 
                       else child `seq`  rnf myself1 `seq` combine [myself1, deLift child] 
                            -- (parMap myself (split x)))
	    where myself = bindivConDAtdm (div numProc 2) (depth +1) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r]  = split x
	          child  = cpAt (selfPe + shift) (process (myself r)) () 
	          shift  = 2 ^ depth
	          myself1 = myself l ()

\end{code}

HACKME
\begin{nocode}
bindivConFlat :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
bindivConFlat numProc trivial solve split combine x
      if numProc <= 1 then deLift (createProcess (process seqDC) x)
                       else if trivial x then solve x 
                       else  l `seq`  left `seq`  r `seq` right `seq` 
                             combine [left, right] 
                            -- (parMap myself (split x)))
	    where myself = bindivConFlat (div numProc 2) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r]  = split x
	          right  =  myself r
	          left   = myself 
\end{nocode}
\begin{code}
bindivConFlat numProc trivial solve split combine x =  = error "hack me!"
\end{code}


only leaf processes, old version (not enough demand)
\begin{code}
bindivConFlat_seq :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
bindivConFlat_seq numProc trivial solve split combine x = 
      if numProc <= 1 then deLift (createProcess (process seqDC) x)
                       else if trivial x then solve x 
                       else  l `seq`  left `seq`  r `seq` right `seq` 
                             combine [left, right] 
                            -- (parMap myself (split x)))
	    where myself = bindivConFlat (div numProc 2) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r]  = split x
	          right  =  myself r
	          left   = myself l
\end{code}

only leaf processes, direct mapping
\begin{code}
bindivConFlatdm :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
bindivConFlatdm numProc trivial solve split combine x = 
      if numProc <= 1 then deLift (createProcess (process (seqDC x)) ())
                       else if trivial x then solve x 
                       else  l `seq`  left `seq`  r `seq` right `seq` 
                             combine [left, right] 
                            -- (parMap myself (split x)))
	    where myself = bindivConFlatdm (div numProc 2) trivial solve split combine
	          seqDC x _ = if trivial x then solve x else combine (map ((flip seqDC) ()) (split x))
	          [l,r]  = split x
	          right  =  myself r
	          left   = myself l
\end{code}

Experimente:
\begin{nocode}
ndivFlatSimple :: (Trans a, Trans b) => 
            (a -> b) -> (a -> [a]) -> ([b] -> b) -> a -> b
-- sequential: ndivFlat seqSolver divide combine = combine . map (seqSolver) . divide
ndivFlatSimple seqSolver divide combine = combine . map(solve) . divide
    where
      solve x = deLift (createProcess (process seqSolver) x)
{-
-- dasselbe mit chunking bereits im skeleton
ndivFlat :: (Trans a, Trans b, Trans c, Trans d) => 
            (b -> c) -> (a -> [b]) -> ([c] -> d) -> a -> d
-- sequential: ndivFlat seqSolver divide combine = combine . map (seqSolver) . divide
ndivFlat seqSolver divide combine = combine' . map(solve) . divide'
    where
      combine' = combine $ map(fftunchunk)
      divide' = (map fftchunk) $ divide
      solve x = deLift (createProcess (process (fftchunk . seqSolver . fftunchunk)) x)
-}
-- dasselbe mit allgemeinem typ, geeignet fürs chunking
ndivFlat :: (Trans a, Trans b, Trans c, Trans d) => 
            (b -> c) -> (a -> [b]) -> ([c] -> d) -> a -> d
ndivFlat seqSolver divide combine = combine . map(solve) . divide
    where
      solve x = deLift (createProcess (process seqSolver) x)

{-
fftDoFlatFreq n = ndivFlatSimple (fft_do' 1) splitFactors  (concat . transpose)         
-}
\end{nocode}