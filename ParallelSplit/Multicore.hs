module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split
import Debug.Trace
import Control.Arrow

instance ParallelSpawn (->) where
    spawn fs = \as -> zipWith ($) fs as `using` parList rdeepseq

--instance (MonadUnwrap m) => ParallelSpawn (Kleisli m) where
--    spawn fs = arr $ \as -> spawn (map runArrow fs) as