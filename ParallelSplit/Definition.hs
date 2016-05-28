{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ParallelSplit.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category
import Control.Parallel.Strategies

import Data.Monoid
import Data.List.Split

type Parrow arr a b = [arr a b]

(...) :: (Arrow arr) => Parrow arr a b -> arr b c -> Parrow arr a c
(...) parr arr = map (>>> arr) parr

(....) :: (Arrow arr) => Parrow arr a b -> Parrow arr b c -> Parrow arr a c
(....) f g = zipWith (>>>) f g

toPar :: (Arrow arr) => arr a b -> Parrow arr a b
toPar arr = [arr]
(<|||>) :: (Arrow arr) => Parrow arr a b -> arr a b -> Parrow arr a b
(<|||>) fs g = fs ++ [g]
(<||||>) :: (Arrow arr) => Parrow arr a b -> Parrow arr a b -> Parrow arr a b
(<||||>) = (++)

(<$$$>) :: (ParallelSpawn arr, ArrowRun arr, NFData b) => Parrow arr a b -> [a] -> [b]
(<$$$>) arr as = runArrow (spawn arr) as

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

parMap :: (ParallelSpawn arr, ArrowRun arr, NFData b) => arr a b -> [a] -> [b]
parMap fn as = runArrow (spawn (replicate (length as) fn)) as