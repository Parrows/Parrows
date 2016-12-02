{-# OPTIONS -cpp #-}
{- Divide and conquer skeleton on top of replicated workers.
   The first parameter of divCon defines the depth at which
   the divide&conquer tree is generated by the main tasks,
   using function generateTasks. After generating enough tasks,
   the replicated workers skeleton is used to solve them with
   a good load balance. Finally, function combineTop combine
   the partial results obtained.
   The data type Tree is needed to pass information from 
   generateTasks to combineTop, so that the subresults can
   be combined in the proper way.   
   
                                              -- FR10 --
-}                                              
   

module DivConRW where
-- (divConRW,divConDM,divConRW2,divConDM_N,divConDM_N_NoRes) where
import Prelude hiding (seq)
import Control.Parallel (pseq)
import Control.Parallel.Strategies (parList,using,NFData)  
import Control.Parallel.Strategies as M (parMap)
import Control.Parallel.Strategies (rdeepseq,Strategy)

--import ParallelSplit.ParMonad
import Control.Arrow
import Parrows.Definition as P
import Parrows.Eden

import Control.Parallel.Eden (Trans)

-- import Eden
-- import RW 

-- workpools by MD
-- import EdenMWToken (mwNest')

-- edi workpool, sorting
-- import EdiWP

import Data.List
import System.IO.Unsafe
import Control.Monad
import Data.List.Split
import Data.Maybe
-- import Observe 

rnf :: NFData a => Strategy a
rnf = rdeepseq -- incorrect, rnf is old signature, but works this way with `using`


seq = pseq 
{-
mw :: (Trans a, Trans b) => 
      Int -> Int -> (a -> b) -> [a] -> [b]
mw = ediWPf

-- farm:
unshuffle :: Int -> [a] -> [[a]]
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]
takeEach :: Int -> [a] -> [a] 
takeEach n [] = []
takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)
shuffle :: [[a]] -> [a]
shuffle = concat . transpose

farm :: (Trans a, Trans b) => Int -> (a -> b) -> [a] -> [b]
farm np f ts = let inputss = unshuffle np ts 
	       in shuffle (parMapAt (repeat 0) (map f) inputss)

parMapAt :: (Trans a, Trans b) => [Int] -> (a -> b) -> [a] -> [b]
parMapAt places f xs
    = unsafePerformIO (
         zipWithM (\pe x -> instantiateAt pe (process f) x)
                  places xs
        )

  -}     
divConSeq :: (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> b
divConSeq trivial solve split combine x 
 | trivial x = solve x
 | otherwise = combine x children
 where children = map (divConSeq trivial solve split combine) (split x)


{-
divConRW :: (Trans a,Trans b, Show b, Show a, NFData b) => -- , Observable b) =>
           Int -> Int -> (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> b
divConRW depth nrTasks trivial solve split combine x 
  = combineTopMaster combine levels results
  where (tasks,levels) = generateTasks depth trivial split x
        results        = workpool (divConSeq trivial solve split combine) tasks
			 --------
	workpool f tasks = mwDM pes 3 nrTasks f tasks
-}

--parMap f xs = map f xs `using` parList rnf

parMapFOrig :: (Trans a, Trans b, NFData b) => (a -> b) -> [a] -> [b]
parMapFOrig = P.parMap ()

parMapFMulticore :: (NFData b) => (a -> b) -> [a] -> [b]
parMapFMulticore = M.parMap rdeepseq

farmChunkF :: (Trans a, Trans b, NFData b) => (a -> b) -> [a] -> [b]
farmChunkF fs as = P.farmChunk () fs 10 4 as

divConRW :: (Trans a, Trans b, NFData a, NFData b) => Int -> Int -> (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> b
divConRW depth _ trivial solve split combine x
 | trivial x = solve x
 | otherwise = children
 where children =
	if depth>0 then
	    -- parallel (dont go down with the depth)
	    combine x $ farmChunkF (divConRW (depth) 0 trivial solve split combine) (split x)
	else
	    -- sequential weiter
	    combine x $ map (divConRW (depth-1) 0 trivial solve split combine) (split x)
            
{-
mwDM :: (Trans a, Trans b) => 
        Int -> Int -> Int-> (a -> b) -> [a] -> [b]
mwDM pes prefetch n f xs = mw pes prefetch (\i -> f (xs!!i)) [0..n-1]



divConDM :: (Trans a,Trans b, Show b, Show a, NFData a, NFData b) => -- 
          -- , Observable (Tree a), Observable a, Observable b) => 
           Int -> (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> b
divConDM depth trivial solve split combine x 
  = -- results `seq` rnf levels `seq`  
    combineTopMaster combine levels results
  where (tasks,levels) = generateTasks depth trivial split x
        results        = shuffle (parMapAt [2,3..]  
                           (\ i -> map (divConSeq trivial solve split combine) 
                                       ((unshuffle pes tasks)!!i)) [0..pes-1])
			 --------
pes :: Int
-- pes = noPe 
pes = max 1 (noPe-1)


-- select the i-th task from tree with b^d tasks  
tasksNr :: [Int] -> (a -> [a]) -> a -> a
tasksNr [] split x = x
tasksNr (i:is) split x = 
          tasksNr is split ((split x)!!i)

digits :: Int -> Int -> Int -> [Int]
digits i d b = reverse $ computeDigits  i d b
computeDigits i d b
  | d == 0 = []
  | otherwise = mod i b : (computeDigits (div i b) (d-1) b)
 
----~
-- divConDM Skeleton which exploits fixed branching degree
divConDM_N :: (Trans a,Trans b, Show b, Show a, NFData a, NFData b) => -- 
          -- , Observable (Tree a), Observable a, Observable b) => 
           Int -> Int ->  
           (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> b
divConDM_N d b trivial solve split combine x 
  = results `seq`  -- rnf levels `seq`  
    combineLevels  b (combine x) results
  where results = -- mw pes 2  
                  -- parMapAt (cycle [2..noPe])-- [2,3..]  
                  farm noPe 
                  (\ i -> divConSeq trivial solve split combine 
                              (tasksNr (digits i d b) split x)) [0..b^d-1]
		
combineLevels ::  NFData b => Int -> ([b] -> b) -> [b] -> b
combineLevels _ _ [x] = x 
combineLevels b combine rs 
 = let levels = map combine (chunkList b rs)
   in  rhead levels `seq` combineLevels b combine $ levels
 
rhead :: NFData a => [a] -> ()
rhead [] = ()
rhead (x:xs) = rnf x 
 
seqList ::  NFData a => (a -> ()) -> [a] -> ()
seqList s []     = ()
seqList s (x:xs) = s x `seq` seqList s xs 


chunkList n [] =  []
chunkList n xs =  ys : chunkList n zs
    where (ys,zs) = splitAt n xs

---- skeleton version without results
-- divConDM Skeleton which exploits fixed branching degree
divConDM_N_NoRes :: (Trans a,Trans b, Show b, Show a, NFData a, NFData b) => -- 
          -- , Observable (Tree a), Observable a, Observable b) => 
           Int -> Int ->  
           (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> [Int]
divConDM_N_NoRes d b trivial solve split combine x 
  = results -- `seq`  -- rnf levels `seq`  
--    combineLevels  b (combine x) results
  where results = -- mw pes 2  
                  -- parMapAt (cycle [1..noPe])-- [2,3..]  
                  farm noPe
                  (\ i -> let r = divConSeq trivial solve split combine 
                                     (tasksNr (digits i d b) split x)
                          in (rnf r `seq` i )) [0..b^d-1]




-----

data Tree a = Tree a [Tree a] | Leaf a  deriving Show
instance NFData a => NFData (Tree a)
 where rnf (Tree a ls) = rnf a `seq` rnf ls 
       rnf (Leaf a)    = rnf a 

-- instance (Observable a) => Observable (Tree a) where
--   observer (Tree a ls) = send "Tree" (return Tree << a << ls)
--   observer (Leaf a)    = send "Leaf" (return Leaf << a)


generateTasks :: Int -> (a->Bool) -> (a->[a]) -> a -> ([a],Tree a)
generateTasks 0 _ _ a = ([a],Leaf a)
generateTasks n trivial split a
 | trivial a = ([a],Leaf a)
 | otherwise = (concat ass,Tree a ts)
 where assts = map (generateTasks (n-1) trivial split) (split a)
       (ass,ts) = unzip assts
                     
foldl_rnf :: NFData a => (a -> b -> a) -> a -> [b] -> a
foldl_rnf _ b [] = b
foldl_rnf f b (x:xs) = let new = f b x
                       in rnf new `seq` foldl_rnf f new xs                 
                     
combineTop :: (NFData b, Show b, Show a) => -- , Observable b) => 
              (a->[b]->b) -> (Tree a) -> [b] -> b
combineTop c t bs = fst (combineTop' c t bs)

combineTopMaster :: (NFData b) => -- ,Observable b) => 
                    (a->[b]->b) -> (Tree a) -> [b] -> b
combineTopMaster c t bs = fst (combineTopRnf c t bs)


#if 1
combineTop' :: (NFData b, Show b, Show a) => -- , Observable b) => 
               (a->[b]->b) -> (Tree a) -> [b] -> (b,[b])
combineTop' _ (Leaf a) (b:bs) = (b,bs)
combineTop' combine (Tree a ts) bs = -- observe "combine" $ 
                                       (combine a (reverse res),bs')
 where (bs',res) = foldl' f (bs,[]) ts
       f (olds,news) t = (remaining,b:news) -- olds sin algo, news mas b
         where (b,remaining) = -- observe "b,remaining" $ 
                                combineTop' combine t olds
combineTop' _ t ls = error ("combineTop' _ Parameter t:\n" ++ show t ++ " \nParameter ls:\n" ++ show ls)  

combineTopRnf :: (NFData b) => -- ,Observable b) => 
                 (a->[b]->b) -> (Tree a) -> [b] -> (b,[b])
combineTopRnf _ (Leaf a) (b:bs) = (b,bs)
combineTopRnf combine (Tree a ts) bs 
 = -- let revres = reverse res 
   -- in  (rnf revres `seq` combine a revres, bs')
   (rnf res `seq` combine a res, bs')
 where (bs',res) = foldl f (bs,[]) ts
       f (olds,news) t = (remaining,news++[b]) -- olds sin algo, news mas b
         where (b,remaining) =  -- observe "Top: b,remaining" $ 
                                combineTopRnf combine t olds



#else
combineTop' :: (a->[b]->b) -> (Tree a) -> [b] -> (b,Int)
combineTop' _ (Leaf a) (b:bs) = (b,1)
combineTop' combine (Tree a ts) bs = (combine a res,length bs - length bs')
 where (bs',res) = foldl f (bs,[]) ts
       f (olds,news) t = (drop n olds,news++[b]) -- olds sin algo, news mas b
         where (b,n) = combineTop' combine t olds
#endif

-------------------------------------------------
-- Mischas MW plus Sortierung
-- mwNest' :: (Trans t, Trans r) =>
--           Int -> Int -> Int -> Int -> (t -> r) -> [t] -> [r]
-- mwNest' depth level1 np pf f tasks
--    = let nesting = mkNesting np depth level1
--      in mwNested' nesting (mkPFs pf nesting) (map f) tasks
-- Jost, Sortierung
-- -- sort results
--             return (map snd (mergewOutsByTag wOuts))
-- sortByTag :: [(Int,t)] -> [t]
-- sortByTag = map snd . sortBy ( \(i,_) (j,_) -> compare i j)

-- waere es nicht besser, die Sortierung in den workpool selbst
-- einzubauen?  dann ginge ein sukzessives merge anstelle von sortBy
-- (allerdings auf jeder Ebene der Hierarchie noetig!)  Evtl. arbeitet
-- das aber auch gar nicht so viel effizienter, je nach Anwendung
-- (erwartet: effizienter wenn Ergebnisse als Stream weiterverarbeitet
-- werden)


liftWorker :: (t -> r) -> (Int, t) -> (Int, r)
liftWorker f (k, x) = (k, f x)

mwNest :: (Trans t, Trans r) =>
          Int -> Int -> Int -> Int -> (t -> r) -> [t] -> [r]
mwNest depth l1 np pf f tasks = let taggedTasks = zip [1,2..] tasks
                                in sortByTag $ mwNest' depth l1 np pf (liftWorker f) taggedTasks

divConRW2 :: (Trans a,Trans b, Show b, Show a, NFData b) => -- , Observable b) => 
            Int -> (a->Bool) -> (a->b) -> (a->[a]) -> (a->[b]->b) -> a -> b
divConRW2 depth trivial solve split combine x 
  = combineTopMaster combine levels results
  where (tasks,levels) = generateTasks depth trivial split x
        results        = workpool (divConSeq trivial solve split combine) tasks
			 --------
workpool f tasks =  mwNest 2 4 noPe 20 f tasks
-}
