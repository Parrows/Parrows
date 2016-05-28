module ParallelSplit.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category
import Control.Parallel.Strategies

import Data.Monoid
import Data.List.Split

class (Monad m) => MonadUnwrap m where
    unwrap :: m a -> a

class (Arrow arr) => ArrowRun arr where
    runArrow :: arr a b -> a -> b

instance ArrowRun (->) where
    runArrow f a = f a

instance (MonadUnwrap m) => ArrowRun (Kleisli m) where
    runArrow (Kleisli f) a = unwrap $ f a

-- now for the Parrow stuff

type Parrow arr a b = [arr a b]

class (Arrow arr) => ParallelSpawn arr where
    spawn :: (NFData b) => Parrow arr a b -> arr [a] [b]

-- some sugar

(...) :: (Arrow arr) => Parrow arr a b -> arr b c -> Parrow arr a c
(...) parr arr = map (>>> arr) parr

(....) :: (Arrow arr) => Parrow arr a b -> Parrow arr b c -> Parrow arr a c
(....) f g = zipWith (>>>) f g

-- minor stuff, remove this?

toPar :: (Arrow arr) => arr a b -> Parrow arr a b
toPar = return
(<|||>) :: (Arrow arr) => Parrow arr a b -> arr a b -> Parrow arr a b
(<|||>) fs g = fs ++ [g]
(<||||>) :: (Arrow arr) => Parrow arr a b -> Parrow arr a b -> Parrow arr a b
(<||||>) = (++)

-- merge a computation, this is basically a parallel zipWith

(<$$>) :: (ParallelSpawn arr, NFData b) => Parrow arr a b -> arr [a] [b]
(<$$>) arr = spawn arr

(<$$$>) :: (ParallelSpawn arr, ArrowRun arr, NFData b) => Parrow arr a b -> [a] -> [b]
(<$$$>) arr as = runArrow (spawn arr) as

-- some skeletons

parZipWith :: (ParallelSpawn arr, ArrowRun arr, NFData b) => Parrow arr a b -> [a] -> [b]
parZipWith = (<$$$>)

parMap :: (ParallelSpawn arr, ArrowRun arr, NFData b) => arr a b -> [a] -> [b]
parMap fn as = parZipWith (replicate (length as) fn) as