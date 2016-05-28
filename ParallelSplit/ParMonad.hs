module ParallelSplit.ParMonad where

import ParallelSplit.Definition
import Control.Monad.Par
import Control.Parallel.Strategies
import Data.List.Split
import Control.Monad
import Control.Arrow

spawn' f x = do y <- spawnP (f x)
                get y

parZipWith' :: (ArrowRun arr, NFData b) => [arr a b] -> arr [a] (Par [b])
parZipWith' fs = arr $ \as -> do
                                 ibs <- zipWithM (\f a -> Control.Monad.Par.spawn $ return ((runArrow f a))) fs as
                                 mapM get ibs

instance ParallelSpawn (->) where
    spawn fs = \as -> runPar $ parZipWith' fs as

instance (MonadUnwrap m) => ParallelSpawn (Kleisli m) where
    spawn fs = arr $ \as -> runPar $ parZipWith' (map runArrow fs) as