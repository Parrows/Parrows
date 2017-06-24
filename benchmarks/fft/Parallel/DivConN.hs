-- divide and conquer skeleton for regular n-ary tree
-- 2008, Jost Berthold, 
-- Philipps-Universitaet Marburg, AG Eden
--------------------------------------------------------
module Parallel.DivConN where

import Parallel.Eden

import System.IO.Unsafe
import Control.Monad
import Control.Parallel.Strategies(seqList,Strategy, rwhnf)


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

spawnAt :: (Trans a, Trans b) => [Int] -> [Process a b] -> [a] -> [b]
spawnAt places ps is = unsafePerformIO
            (sequence
             [instantiateAt st p i |
              (st,p,i) <- zip3 (cycle places) ps is]
            )

          

-- distributed expansion (tree-shaped process creation, one tasks stays local)
dcN :: (Trans a, Trans b) => 
       Int -> Int                 -- n (expect n children) / remaining depth
       -> (a -> Bool) -> (a -> b) -- trivial? / solve
       -> (a -> [a]) -> ([b] -> b) -- split / combine
       ->  a -> b                 -- resulting mapping
dcN n depth trivial solve split combine x 
   = dcN_c n depth trivial solve split (\_ parts -> combine parts) x

dcN_c :: (Trans a, Trans b) => 
       Int -> Int                 -- n (expect n children) / remaining depth
       -> (a -> Bool) -> (a -> b) -- trivial? / solve
       -> (a -> [a]) -> (a -> [b] -> b) -- split / combine (incl. input)
       ->  a -> b                 -- resulting mapping
dcN_c n depth trivial solve split combine x 
   = if depth < 1 then seqDC x
     else if trivial x then solve x 
                       else childRs `seq` -- early demand on children list
                            combine x (myR : childRs)
	    where myself = dcN_c n (depth - 1) trivial solve split combine
	          seqDC x     = if trivial x then solve x 
			                     else combine x (map seqDC (split x))
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

---------------------------------------------------------------
-- interface using PE tickets
dcNTickets :: (Trans a, Trans b) => 
       Int -> [Int]          -- n (expect n children) / Tickets
       -> (a -> Bool) -> (a -> b)  -- trivial? / solve
       -> (a -> [a]) -> ([b] -> b) -- split / combine
       -> (a -> b)                 -- seqDC
       ->  a -> b                  -- resulting mapping
dcNTickets k ts trivial solve split combine seqDC x 
 = dcNTickets_c k ts trivial solve split (\_ parts -> combine parts) seqDC x

dcNTickets_c :: (Trans a, Trans b) => 
       Int -> [Int]          -- n (expect n children) / Tickets
       -> (a -> Bool) -> (a -> b)  -- trivial? / solve
       -> (a -> [a]) -> (a -> [b] -> b) -- split / combine (incl. input)
       -> (a -> b)                 -- seqDC
       ->  a -> b                  -- resulting mapping
dcNTickets_c k [] trivial solve split combine seqDC x = seqDC x
dcNTickets_c k tickets trivial solve split combine seqDC x  
   = if trivial x then solve x 
                  else childRes `seq` -- early demand on children list
		       rnf myRes `seq`
                            combine x (myRes:childRes ++ localRess )
        where
          -- splitting computation into processes
          (childTickets,restTickets) = splitAt (k-1) tickets
          (myTs:theirTs)=unshuffle k restTickets
          ticketF ts = dcNTickets_c k ts trivial solve split combine seqDC 
          insts = length childTickets
          (procIns, localIns) = splitAt insts theirIn 
          childProcs = map (process . ticketF) theirTs 
          childRes  = spawnAt childTickets childProcs procIns

          -- local computation:
          myRes = ticketF myTs myIn
          (myIn:theirIn) = split x
          localRess = map seqDC localIns

          -- move to another module?
          unshuffle :: Int -> [a] -> [[a]]
          unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]
          takeEach :: Int -> [a] -> [a] 
          takeEach n [] = []
          takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)

---------------------------------------------------------------
dc4WithSeq :: (Trans a, Trans b) =>
        Int -> -- PEs to use
	(a -> Bool) -> (a -> b) ->   -- param fct.s
	(a -> [a]) -> ([b] -> b) ->  -- param fct.s
	(a -> b) ->      -- seqDC
	a -> b
dc4WithSeq pes = quadDCAtSep (logN 4 pes)

-- separate sequential version
quadDCAtSep :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) -> (a -> b) ->  a -> b
quadDCAtSep depth trivial solve split combine seqDC x = 
      if depth < 1 then seqDC x
                       else if trivial x then solve x 
                       else -- children `seq`  
		            seqList rwhnf childrenL `seq`
			    combine (myself1 : children)
	    where myself = quadDCAtSep (depth - 1) trivial solve split combine seqDC
	          [l,r1,r2,r3]  = split x
	          children  = -- parMapAt places myself [r1,r2,r3]
		              map deLift childrenL
		  childrenL = zipWith(\arg place -> cpAt place (process (myself)) arg)
                                        [r1,r2,r3] places
		  places = map ((+1) . (`mod` noPe) . (+(-1))) shifts
                  shifts = map (selfPe +) [shift,2*shift..]
                  shift  = 4 ^ (depth -1)
	          myself1 = myself l
