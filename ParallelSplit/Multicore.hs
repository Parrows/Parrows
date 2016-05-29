module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split
import Debug.Trace
import Control.Arrow
import Control.Monad

instance ParallelSpawn (->) where
    spawn fs = \as -> zipWith ($) fs as `using` parList rdeepseq

instance (MonadUnwrap m) => ParallelSpawn (Kleisli m) where
    spawn = (arr $ \fs -> Kleisli $ \as -> sequence (zipWith (\f a -> runKleisli f a) fs as `using` parList rdeepseq))