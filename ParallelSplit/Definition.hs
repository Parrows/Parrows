{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ParallelSplit.Definition where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List.Split
import Control.Category
import Control.Arrow
import Data.Monoid

type Parrow arr a b = [arr a b]

(...) :: (Arrow arr) => Parrow arr a b -> arr b c -> Parrow arr a c
(...) parr arr = map (>>> arr) parr

(....) :: (Arrow arr) => Parrow arr a b -> Parrow arr b c -> Parrow arr a c
(....) f g = zipWith (>>>) f g

toPar :: (Arrow arr) => arr a b -> Parrow arr a b
toPar arr = [arr]
(<|||>) :: (Arrow arr) => Parrow arr a b -> arr a b -> Parrow arr a b
(<|||>) (fs) g = fs `mappend` [g]
(<||||>) :: (Arrow arr) => Parrow arr a b -> Parrow arr a b -> Parrow arr a b
(<||||>) = mappend

-- TODO: merge operation?

class (Monad m) => MonadUnwrap m where
    unwrap :: m a -> a

class (Arrow arr) => ArrowRun arr where
    runArrow :: arr a b -> a -> b

instance ArrowRun (->) where
    runArrow f a = f a

instance (MonadUnwrap m) => ArrowRun (Kleisli m) where
    runArrow (Kleisli f) a = unwrap $ f a

class (Arrow arr) => ParallelSpawn arr where
    spawn :: (NFData b) => Parrow arr a b -> arr [a] [b]

--default implementation without threading
instance (ArrowRun arr) => ParallelSpawn arr where
    spawn fs = arr $ zipWith runArrow fs

parMap :: (ArrowRun arr, NFData b) => arr a b -> [a] -> [b]
parMap fn as = runArrow (spawn (replicate (length as) fn)) as