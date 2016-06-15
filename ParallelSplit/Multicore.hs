{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split
import Control.Arrow
import Control.Monad

monadStrat :: (Monad m, NFData a) => Strategy (m a)
monadStrat m = return $ m >>= makeStrict

instance (ArrowApply arr, ArrowChoice arr) => ParallelSpawn arr where
    parEvalN = arr $ \fs -> ((arr $ \as -> zipWith (,) fs as) >>> listApp >>> (arr $ \bs -> bs `using` parList rdeepseq))