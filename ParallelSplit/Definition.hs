{-# LANGUAGE FlexibleInstances #-}
module ParallelSplit.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category

import Data.Monoid
import Data.List.Split

type Parrow arr a b = [arr a b]

class (Arrow arr) => ParallelSpawn arr where
    spawn :: (NFData b) => arr (Parrow arr a b) (arr [a] [b])

class (Monad m) => MonadUnwrap m where
    unwrap :: m a -> a

instance (NFData b, MonadUnwrap m) => (NFData (m b)) where
    rnf mon = rnf (unwrap mon)

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

(<$$>) :: (ParallelSpawn arr, NFData b) => arr (Parrow arr a b) (arr [a] [b])
(<$$>) = spawn

-- some skeletons

parEval :: (ParallelSpawn arr, NFData b) => arr (Parrow arr a b) (arr [a] [b])
parEval = (<$$>)

{-
parZipWith :: (ParallelSpawn arr, ArrowApply arr, NFData c) => arr (a, b) c -> [a] -> (arr [b] [c])
parZipWith fn as = arr $ \bs -> app (_, bs)
-}

--parMap :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (arr a b, [a]) [b]
--parMap = (arr $ \(fn, as) ->
--                let fns = replicate (length as) fn
--                in (spawn fns, as)) >>> app


parMap :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (arr a b, [a]) [b]
parMap = (arr $ \(fn, as) -> (replicate (length as) fn, as)) >>> (first parEval) >>> app