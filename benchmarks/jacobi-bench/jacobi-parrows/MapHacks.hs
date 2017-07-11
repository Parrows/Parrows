
{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Eden.EdenSkel.MapSkels
-- Copyright   :  (c) Philipps Universitaet Marburg 2009-2010
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  eden@mathematik.uni-marburg.de
-- Stability   :  beta
-- Portability :  not portable
--
-- This Haskell module defines map-like skeletons for 
-- the parallel functional language Eden.
--
-- Depends on the Eden Compiler.
--
-- Eden Project


module MapHacks (
                 -- * Custom map skeletons 
                 -- | These skeletons expose all parameters to the user and are thus of varying types
                 parMap, farm, farm', ssf,
                 -- * Simple map skeleton variants 
                 -- | The map skeletons (parMap, farm, ssf) can be used to define 
                 --  skeletons with the simpler sequential map interface :: (a -> b) -> [a] -> [b]
                 map_par, map_farm, map_ssf, map_wp,
                 -- * Skeletons for dynamic task distribution 
                 -- | The workpool skeletons use the non-deterministic merge function to achieve dynamic load balancing.
                  workpool, workpoolAux, workpoolSorted', workpoolSortedNonBlock'
                 ) where
#if defined( __PARALLEL_HASKELL__ )
import Control.Parallel.Eden
#else
import Control.Parallel.Eden.EdenConcHs
#endif
import Control.Parallel.Eden.Auxiliary
import Data.List
import Data.Maybe(maybeToList,mapMaybe)

import System.IO.Unsafe
import Control.Monad

import Control.Concurrent


-- | Basic parMap Skeleton - one process for each list element
parMap' :: (Trans a, Trans b) => 
                (a -> b)   -- ^worker function
                -> [a]     -- ^task list
                -> [b]     -- ^result list
parMap' f tasks = spawn (repeat (process f)) tasks

spawn' :: (Trans a, Trans b)
      => [a -> b] -- ^ list of worker functions
      -> [a]      -- ^ task list
      -> [b]      -- ^ result list
spawn' fs xs = unsafePerformIO $ zipWithM instantiate (map process fs) xs

parMap :: (Trans a, Trans b) 
       => (a -> b) -- ^ worker function
       -> [a]      -- ^ task list
       -> [b]      -- ^ result list
parMap f xs = spawn' (repeat f) xs


-- | A farm distributes its input to a number of worker processes.
-- The distribution function is expected to divide the input list into 
-- the given number of sublists. The results of the worker processes are 
-- then merged using the combination function.
-- 
-- Use 'map_farm' if you want a simpler interface.
--
farm'' :: (Trans a, Trans b) => 
                Int                      -- ^number of child processes
                -> (Int -> [a] -> [[a]]) -- ^input distribution function 
                -> ([[b]] -> [b])        -- ^result combination function
                -> ([a] ->  [b])         -- ^worker function (for each sub list)
		-> [a]                   -- ^input 
		-> [b]                   -- ^output

farm'' np distr combine f tasks = 
        combine (parMap f (distr np tasks))


farmBase :: (Trans a, Trans b)
         => ([a] -> [[a]]) -- ^ input distribution function 
         -> ([[b]] -> [b]) -- ^ result combination function
         -> (a ->  b)      -- ^ worker function
         -> [a]            -- ^ input 
         -> [b]            -- ^ output

farmBase distr comb f tasks = comb $ parMap (map f) $ distr tasks

farm' :: (Trans a, Trans b)
     => Int     -- ^ number of child processes
     -> Map a b -- ^ map type signature
farm' n = farmBase (unshuffle n) shuffle 

farm :: (Trans a, Trans b) => Map a b -- compatible to map
farm = farm' (max 1 (noPe-1))

type Map a b = (a -> b) -> [a] -> [b]

-- | Self service farm (alias direct mapping): Like the farm, but tasks are evaluated in
-- the workers (less communication overhead). Tasks are mapped inside 
-- each generated Process abstraction avoiding evaluation prior to sending. This often 
-- reduces the communication overhead because unevaluated data is usually much smaller 
-- than evaluated data. 
-- 
-- Use 'map_ssf' if you want a simpler interface.
--
-- Notice: The task lists structure has to be completely defined before process 
-- instantiation takes place.
--
ssf :: forall a b . (Trans a, Trans b) => 

                Int                          -- ^number of child processes
                -> (Int -> [a] -> [[a]])     -- ^input distribution function 
                -> ([[b]] -> [b])            -- ^result combination function
                -> ([a] ->  [b])             -- ^worker function (processes each sub list)
		-> [a]                       -- ^input 
		-> [b]                       -- ^output

ssf  np distribute combine f xs 	
   =  combine ( 
          spawn [ pf (tasks i) | i <- [0..(np-1)]]
                     (replicate np ()))
       where  tasks i  = (distribute np xs) !! i
              pf :: (Trans a, Trans b) => 
                    [a] -> Process () [b]
              pf x = process (\_ -> f x)
------------------------------------------------------------------------------------
map_par, map_farm, map_ssf:: (Trans a , Trans b) => 
                             (a -> b)   -- ^worker function
                             -> [a]     -- ^task list
                             -> [b]     -- ^result list 
map_par    = parMap' 
map_farm f = farm'' (max 1 (noPe - 1)) unshuffle shuffle (map f)
map_ssf f  = ssf (max 1 (noPe - 1))  unshuffle shuffle (map f)

-- | Simple workpool (result list in non-deterministic order)
--
-- Notice: Each single task has to be transformed to exactly one result, 
-- otherwise the skeletons behaviour is not as expected
workpool :: forall t r . (Trans t, Trans r) =>
	    Int              -- ^number of child processes (workers)
	    -> Int           -- ^prefetch of tasks (for workers)
	    -> ([t] -> [r])  -- ^worker function (tasks to results mapping) 
	    -> [t] -> [r]    -- ^what to do
workpool np prefetch worker tasks
   = map snd fromWorkers
    where   
            fromWorkers :: Trans r => [(Int,r)]
            fromWorkers     = merge (tagWithPids (parMap worker taskss))
            taskss :: Trans t => [[t]]
            taskss          = distribute np (initialReqs ++ newReqs) tasks
            initialReqs, newReqs :: [Int]
            initialReqs     = concat (replicate prefetch [0..np-1])
            newReqs         = map fst fromWorkers

map_wp :: (Trans t, Trans r)
       => Map t r 
map_wp f = workpool noPe 2 (map f)

-- | Workpool version with one result stream for each worker and meta information about the task distribution. 
--
-- This meta-skeleton can be used to define workpool-skeletons which can reestablish the result list order. 
--
--  Notice: Each single task has to be transformed to exactly one result, otherwise the skeletons behaviour is not as expected
workpoolAux :: (Trans t, Trans r) =>
	    Int                      -- ^number of child processes (workers)                                                                        
	    -> Int                   -- ^prefetch of tasks (for workers)                                                                            
	    -> ([t] -> [r])          -- ^worker function (tasks to results mapping)                                                         
            -> [t]                   -- ^inputs
            -> ([Int],[[Int]],[[r]]) -- ^(input distribution (input i is in sub-list distribs!i), task positions (element i of result-sub-list j was in the input list at (poss!j)!i ), result streams of workers)
workpoolAux np prefetch worker tasks
   = (reqs,poss,fromWorkers)
    where   fromWorkers     = parMap worker taskss
            (taskss,poss)   = distributeWithPos np reqs tasks
            -- generate only as many reqs as there are tasks
            reqs            = map snd $ zip tasks $ initialReqs ++ newReqs
            initialReqs     = concat (replicate prefetch [0..np-1])
            newReqs         = merge $ mkPids fromWorkers

workpoolAux' :: (Trans t, Trans r) =>
            Int                      -- ^number of child processes (workers)                                                                        
            -> Int                   -- ^prefetch of tasks (for workers)                                                                            
            -> (t -> r)          -- ^worker function (tasks to results mapping)                                                         
            -> [t]                   -- ^inputs
            -> IO ([Int],[[Int]],[[r]]) -- ^(input distribution (input i is in sub-list distribs!i), task positions (element i of result-sub-list j was in the input list at (poss!j)!i ), result streams of workers)
workpoolAux' np prefetch worker tasks
   = return (reqs,poss,fromWorkers)
    where   fromWorkers     = parMap (map worker) taskss
            (taskss,poss)   = distributeWithPos np reqs tasks
            -- generate only as many reqs as there are tasks
            reqs            = map snd $ zip tasks $ initialReqs ++ newReqs
            initialReqs     = concat (replicate prefetch [0..np-1])
            newReqs         = merge $ mkPids fromWorkers
            --

-- | Sorted workpool (results in the order of the tasks)
--
--  Notice: Each single task has to be transformed to exactly one result, 
--  otherwise the skeletons behaviour is not as expected
workpoolSorted  :: (Trans t, Trans r) =>
	    Int             -- ^number of child processes (workers)
	    -> Int          -- ^prefetch of tasks (for workers)
            -> ([t] -> [r]) -- ^worker function (tasks to results mapping) 
	    -> [t] -> [r]   -- ^what to do
workpoolSorted np prefetch f tasks = res 
    where (_, poss, ress) = workpoolAux np prefetch f tasks
          res = map snd $ mergeByPos ress'
          ress' = map (uncurry zip) (zip poss ress)

workpoolSorted'  :: (Trans t, Trans r) =>
            Int             -- ^number of child processes (workers)
            -> Int          -- ^prefetch of tasks (for workers)
            -> (t -> r) -- ^worker function (tasks to results mapping) 
            -> [t] -> [r]   -- ^what to do
workpoolSorted' np prefetch f tasks = res
    where (_, poss, ress) = unsafePerformIO $ workpoolAux' np prefetch f tasks
          res = map snd $ mergeByPos ress'
          ress' = map (uncurry zip) (zip poss ress)


-- | Non-blocking sorted workpool (results in the order of the tasks). Result 
-- list is structurally defined up to the position where tasks are distributed, independent 
-- of the received worker results .This version needs still performance testing.
--
--  Notice: Each single task has to be transformed to exactly one result, 
--  otherwise the skeletons behaviour is not as expected
workpoolSortedNonBlock  :: (Trans t, Trans r) =>
	    Int             -- ^number of child processes (workers)
	    -> Int          -- ^prefetch of tasks (for workers)
            -> ([t] -> [r]) -- ^worker function (tasks to results mapping) 
	    -> [t] -> [r]   -- ^what to do
workpoolSortedNonBlock np prefetch f tasks 
  = orderBy fromWorkers reqs
   where (reqs, _ ,fromWorkers) = workpoolAux np prefetch f tasks

workpoolSortedNonBlock'  :: (Trans t, Trans r) =>
            Int             -- ^number of child processes (workers)
            -> Int          -- ^prefetch of tasks (for workers)
            -> (t -> r) -- ^worker function (tasks to results mapping) 
            -> [t] -> [r]   -- ^what to do
workpoolSortedNonBlock' np prefetch f tasks
  = orderBy fromWorkers reqs
   where (reqs, _ ,fromWorkers) = unsafePerformIO $ workpoolAux' np prefetch f tasks

