4-ariges Baum.

\begin{code}
module Parallel.Skel.Quad {-(dc, parmap)-} where

import Parallel.Eden
-- import Parallel.Skel.Trivial as Trivial (dc)
import System.IO.Unsafe (unsafePerformIO) -- use new interface!
import qualified Parallel.Strategies as PS 
-- import Prelude (max)
\end{code}

\begin{code}

\end{code}

Gehoert das ins Skelett rein?!
\begin{code}
import MathObj.Vector.Indexed
import MathObj.Vector.Chunked
\end{code}

Das verwenden wir:
\begin{code}
-- dc = quadDivConFlatDMC -- nicht flat!
dc = quaddivConDPC
\end{code}

Ein einfaches Skeleton. Verhaellt sich ziehmlich schlecht.
\begin{code}
quaddivConD :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quaddivConD numProc trivial solve split combine x = 
      if numProc <= 0 then seqDC x
                       else if trivial x then solve x 
                       else children `seq`  myself1 `seq` combine (myself1 : children)
                            -- (parMap myself (split x)))
	    where myself = quaddivConD (div numProc 2) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          (l:rs)  = split x
                  children = parmap (myself) rs
	          myself1 = myself l
\end{code}

Einfach parmap? Die ganze Arbeit ist in merge, mege kann parmapt werden.
\begin{code}
parmap f (x:xs) = let res = map (createProcess (process f)) xs 
                  in (f x):map deLift res
\end{code}

Ein besseres divcon?
with additional depth control parameter and explicit placement
\begin{code}
quaddivConDP :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quaddivConDP n trivial solve split combine x 
    -- = bindivConDAtdm n 0 trivial solve split combine x ()
    = quaddivConDAt' n 0 trivial solve split combine x

quaddivConDAt' :: (Trans a, Trans b) => 
           Int -> Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quaddivConDAt' numProc depth trivial solve split combine x = 
         -- neue logik: tiefe vorberechnen und absteigend in rekursion
         let d = log4 numProc
             log4 1 = 0
             log4 n | n > 0 = 1 + log4 ((n + 3)`div` 4)
                    | otherwise = error "log4"
         in  quaddivConDAt d trivial solve split combine x 
quaddivConDAt :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quaddivConDAt depth trivial solve split combine x = 
      if depth < 1 then seqDC x
                       else if trivial x then solve x 
                       else PS.seqList PS.rwhnf children `seq`  
		            rnf r1 `seq` rnf r2 `seq` rnf r3 `seq`
			    combine (myself1 : map(deLift) children)
                            -- (parMap myself (split x)))
	    where myself = quaddivConDAt (depth - 1) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r1,r2,r3]  = split x
	          children  = zipWith(\arg place -> cpAt place (process (myself)) arg) [r1,r2,r3] places
		  places = map ((+1) . (`mod` noPe) . (+(-1))) shifts
                  shifts = map (selfPe +) [shift,2*shift..]
                  shift  = 4 ^ (depth -1)
	          myself1 = myself l
\end{code}
\begin{nocode}
quaddivConDP :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quaddivConDP n trivial solve split combine x 
    -- = bindivConDAtdm n 0 trivial solve split combine x ()
    = quaddivConDAt n 0 trivial solve split combine x

quaddivConDAt :: (Trans a, Trans b) => 
           Int -> Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quaddivConDAt numProc depth trivial solve split combine x = 
      if numProc <= 1 then seqDC x
                       else if trivial x then solve x 
                       else PS.seqList PS.rwhnf children `seq`  
		            -- rnf myself1
			    combine (myself1 : map(deLift) children)
                            -- (parMap myself (split x)))
	    where myself = quaddivConDAt (div numProc 3) (depth +1) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
	          [l,r1,r2,r3]  = split x
	          children  = zipWith(\arg placement -> cpAt (selfPe + shift + placement) (process (myself)) arg) [r1,r2,r3] [0..2]
	          shift  = 3 ^ depth
	          myself1 = myself l
\end{nocode}
rewrite with parmap?

das quaddivConDP mit Chunking:
\begin{code}
-- quaddivConDPC :: (Trans a, Trans d) => 
--                  Int -> Int -> (a -> Bool) -> (a -> d) -> (a -> [a]) -> 
-- 		 ([d] -> d) ->  a -> d
quaddivConDPC n k isTrivial solve split combine x 
    = unchunk $ quaddivConDAt' n 0 isTrivial' solve' split' combine' x'
              where solve' = cLift1 k solve
                    split' = cLift1N k split
                    combine' = cLiftN1 k combine
                    isTrivial' = isTrivial . unchunk
                    x' = chunk k x
\end{code}

Die DM-versionen. Aufbauen auf diesem Code:

\begin{nocode}
-- only leaf processes, direct mapping
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
\end{nocode}

\begin{code}
-- only leaf processes, direct mapping
-- quadDivConFlatDM :: (Trans a, Trans b) => 
--           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) ->  a -> b
quadDivConFlatDM numProc trivial solve split combine x = 
      if numProc <= 1 then deLift (createProcess (process (seqDC x)) ())
                       else if trivial x then solve x 
                       else  l `seq`  left `seq`  rs `seq` rights `seq` 
                             combine (left:rights) 
                            -- (parMap myself (split x)))
	    where myself = quadDivConFlatDM (div numProc 4) trivial solve split combine
	          seqDC x _ = if trivial x then solve x else combine (map ((flip seqDC) ()) (split x))
	          l:rs  = split x
	          rights  =  map (myself) rs
	          left   = myself l
\end{code}
TODO: umschreiben `left:rights` in `children`.

DM Skelett mit chunking:
\begin{code}
quadDivConFlatDMC n k isTrivial solve split combine x 
    = unchunk $ quadDivConFlatDM n isTrivial' solve' split' combine' x'
              where solve' = cLift1 k solve
                    split' = cLift1N k split
                    combine' = cLiftN1 k combine
                    isTrivial' = isTrivial . unchunk
                    x' = chunk k x
\end{code}
