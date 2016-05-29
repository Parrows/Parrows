module ParallelSplit.ParMonad where

import ParallelSplit.Definition
import Control.Monad.Par
import Control.Parallel.Strategies
import Data.List.Split
import Control.Monad
import Control.Arrow

spawn' f x = do y <- spawnP (f x)
                get y

parEval' :: (NFData b) => [a -> b] -> [a] -> (Par [b])
parEval' fs = arr $ \as -> do
                                 ibs <- zipWithM (\f a -> Control.Monad.Par.spawn $ return $ f a) fs as
                                 mapM get ibs

instance ParallelSpawn (->) where
    spawn fs = \as -> runPar $ parEval' fs as

--instance (MonadUnwrap m) => ParallelSpawn (Kleisli m) where
--    spawn = (arr $ \fs -> Kleisli $ \as -> runPar $ parEval' (map unwrap fs) as)