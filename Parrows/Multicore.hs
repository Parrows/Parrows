{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Parrows.Multicore where

import Parrows.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Control.Arrow

instance (ArrowApply arr, ArrowChoice arr) => ParallelSpawn arr where
    parEvalN = arr $ \fs -> ((arr $ \as -> zipWith (,) fs as) >>> listApp >>> (arr $ \bs -> bs `using` parList rdeepseq))