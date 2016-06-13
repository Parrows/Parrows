module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split
import Debug.Trace
import Control.Arrow
import Control.Monad

monadStrat :: (MonadStrict m, NFData a) => Strategy (m a)
monadStrat m = return $ strict m

instance ParallelSpawn (->) where
    parEvalN fs = \as -> zipWith ($) fs as `using` parList rdeepseq

instance (MonadStrict m) => ParallelSpawn (Kleisli m) where
    parEvalN = (arr $ \fs -> Kleisli $ \as -> sequence ((zipWith (\f a -> (runKleisli f a)) fs as ) `using` parList monadStrat ))