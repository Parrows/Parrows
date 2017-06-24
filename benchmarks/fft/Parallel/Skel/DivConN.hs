-- divide and conquer skeleton for regular n-ary tree
-- 2008, Jost Berthold, 
-- Philipps-Universitaet Marburg, AG Eden
--------------------------------------------------------
module Parallel.Skel.DivConN where

import Parallel.Eden

import System.IO.Unsafe
import Control.Monad
import Control.Parallel.Strategies(seqList,Strategy)


{-
parMapEden :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
parMapEden f xs = map deLift ([ createProcess (process f) x
                               | x <- xs ] `using` whnfspine)
-- demand control helper
whnfspine :: Strategy [a]
whnfspine [] = ()
whnfspine (x:xs) = x `seq` whnfspine xs

parMapEden2 :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
parMapEden2 f xs = unsafePerformIO ( mapM (instantiate (process f)) xs )
-}

-- Eden parmap, placement:
parMapAt :: (Trans a, Trans b) => [Int] -> (a -> b) -> [a] -> [b]
parMapAt places f xs
    = unsafePerformIO (
         zipWithM (\pe x -> instantiateAt pe (process f) x)
                  places xs
        )


-- distributed expansion (tree-shaped process creation, one tasks stays local)
dcN :: (Trans a, Trans b) => 
       Int -> Int                 -- n (expect n children) / remaining depth
       -> (a -> Bool) -> (a -> b) -- trivial? / solve
       -> (a -> [a]) -> ([b] -> b) -- split / combine
       ->  a -> b                 -- resulting mapping
dcN n depth trivial solve split combine x 
   = if depth < 1 then seqDC x
     else if trivial x then solve x 
                       else childRs `seq` -- early demand on children list
                            combine (myR : childRs)
	    where myself = dcN n (depth - 1) trivial solve split combine
	          seqDC x     = if trivial x then solve x 
			                     else combine (map seqDC (split x))
	          (mine:rest) = split x
		  myR = myself mine
		  childRs = parMapAt places myself rest
			      `using` seqList r0 -- ???
		  -- placement with stride for next children, round-robin
		  places = map ((+1) . (`mod` noPe) . (+(-1))) shifts
                  shifts = map (selfPe +) [shift,2*shift..]
                  shift  = n ^ (depth -1)

-- chunking version: => input/output restricted to lists
--                   => problem-dependent, encode chunking in worker function!



-- rounding-up log approximation
logN n 1 = 0
logN n  k | k > 0 = 1 + logN n ((k + n-1) `div` n) -- round up
          | otherwise = error "logN"

-- interface with noPe:
dcN' n pes = dcN n depth
  where depth = logN n pes
