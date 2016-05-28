module NewDC where

import ParallelSplit.ParMonad
import ParallelSplit.Definition
import Control.Parallel.Strategies hiding (parMap)
import Data.List
import Control.Parallel
import Control.DeepSeq (rnf)
import Debug.Trace

parMap' :: (NFData b) => (a -> b) -> [a] -> [b]
parMap' f as = parMap f as

-- old Eden code below, has Trans context removed, but needs NFData context. added

-- Code from MapHacks.hs, new farm from https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/src/Control-Parallel-Eden-Map.html#farm might be better, but I doubt it
farmBase :: NFData b => ([a] -> [[a]]) -- ^ input distribution function
         -> ([[b]] -> [b]) -- ^ result combination function
         -> (a ->  b)      -- ^ worker function
         -> [a]            -- ^ input
         -> [b]            -- ^ output

farmBase distr comb f tasks = comb $ parMap' (map f) $ distr tasks

farm' :: NFData b => Int     -- ^ number of child processes
     -> ((a -> b) -> [a] -> [b]) -- ^ map type signature
farm' n = farmBase (unshuffle n) shuffle

-- hack!!!
num_cores = 8
farm :: NFData b => (a -> b) -> [a] -> [b]
farm = farm' num_cores

-- helpers
-- see also https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/src/Control-Parallel-Eden-Auxiliary.html#unshuffle
-- | Round robin distribution - inverse to shuffle
--
unshuffle :: Int      -- ^number of sublists
             -> [a]   -- ^input list
             -> [[a]] -- ^distributed output
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]

takeEach :: Int -> [a] -> [a]
takeEach n [] = []
takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)


-- | Simple shuffling - inverse to round robin distribution
shuffle :: [[a]]  -- ^ sublists
           -> [a] -- ^ shuffled sublists
shuffle = concat . transpose

-- Code from https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/src/Control-Parallel-Eden-DivConq.html
--   Divide-and-conquer scheme
type DivideConquer a b
  = (a -> Bool)        -- ^ trivial?
    -> (a -> b)        -- ^ solve
    -> (a -> [a])      -- ^ split
    -> (a -> [b] -> b) -- ^ combine
    -> a               -- ^ input
    -> b               -- ^ result

-- | Sequential Version.
dc :: DivideConquer a b
dc trivial solve split combine = rec_dc
  where
    rec_dc x = if trivial x then solve x
               else combine x (map rec_dc (split x))

-- | Simple parMap parallelisation with depth control but
-- no placement control. This variant allows to
-- give an additional depth parameter for the recursion, proceeding in a
-- sequential manner when @depth=0@. The process scheme unfolds the call
-- tree on processors chosen by the runtime environment. Round-Robin
-- distribution is unfavourable for this skeleton, better use RTS option
-- @+RTS -qrnd@ when using it.
parDC :: NFData b => Int            -- ^ parallel depth
         -> DivideConquer a b
parDC lv trivial solve split combine
 = pdc lv
 where
  pdc lv x
   | lv == 0 = dc trivial solve split combine x
   | lv >  0 = if trivial x then solve x
               else combine x (parMap' (pdc (lv-1)) (split x))

-------------------------------Flat Expansion----------------------------------
-- | DC Skeleton with flat expansion of upper DC-tree levels, takes custom map
-- skeletons to solve expanded tasks (a sequential map skeleton leads to a
-- sequential DC-skeleton).
flatDC :: NFData b
                => ((a->b)->[a]->[b]) -- ^custom map implementation
                -> Int             -- ^depth
                -> DivideConquer a b
flatDC myMap depth trivial solve split combine x
  = combineTopMaster combine levels results
  where (tasks,levels) = generateTasks depth trivial split x
        results        = myMap(dc trivial solve split combine) tasks -- OL: fixed skeleton name here



combineTopMaster :: (NFData b) =>
                    (a->[b]->b) -> (Tree a) -> [b] -> b
combineTopMaster c t bs = fst (combineTopRnf c t bs)


combineTopRnf :: (NFData b) =>
                 (a->[b]->b) -> (Tree a) -> [b] -> (b,[b])
combineTopRnf _ (Leaf a) (b:bs) = (b,bs)
combineTopRnf combine (Tree a ts) bs
 = (rnf res `pseq` combine a res, bs')
 where (bs',res)      = foldl f (bs,[]) ts
       f (olds,news) t = (remaining,news++[b])
         where (b,remaining) =  combineTopRnf combine t olds


generateTasks :: Int -> (a->Bool) -> (a->[a]) -> a -> ([a],Tree a)
generateTasks 0 _ _ a = ([a],Leaf a)
generateTasks n trivial split a
 | trivial a = ([a],Leaf a)
 | otherwise = (concat ass,Tree a ts)
 where assts = map (generateTasks (n-1) trivial split) (split a)
       (ass,ts) = unzip assts


data Tree a = Tree a [Tree a] | Leaf a  deriving Show
instance NFData a => NFData (Tree a)
 where rnf (Tree a ls) = rnf a `seq` rnf ls
       rnf (Leaf a)    = rnf a
