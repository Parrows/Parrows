module ParallelSplit.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category

import Data.Monoid
import Data.List.Split
import Data.List

type Parrow arr a b = [arr a b]

class (Arrow arr) => ParallelSpawn arr where
    spawn :: (NFData b) => arr (Parrow arr a b) (arr [a] [b])

class (Monad m) => MonadUnwrap m where
    unwrap :: m a -> a

-- some sugar

(...) :: (Arrow arr) => Parrow arr a b -> arr b c -> Parrow arr a c
(...) parr arr = map (>>> arr) parr

(....) :: (Arrow arr) => Parrow arr a b -> Parrow arr b c -> Parrow arr a c
(....) f g = zipWith (>>>) f g

-- behaves like <*> on lists and combines them with (....)

(<*....>) :: (Arrow arr) => Parrow arr a b -> Parrow arr b c -> Parrow arr a c
(<*....>) fs gs = concat $ zipWith (....) (repeat fs) (permutations gs)

-- minor stuff, remove this? these are basically just operations on lists

toPar :: (Arrow arr) => arr a b -> Parrow arr a b
toPar = return
(<|||>) :: (Arrow arr) => Parrow arr a b -> arr a b -> Parrow arr a b
(<|||>) fs g = fs ++ [g]
(<||||>) :: (Arrow arr) => Parrow arr a b -> Parrow arr a b -> Parrow arr a b
(<||||>) = (++)

(<$$>) :: (ParallelSpawn arr, NFData b) => arr (Parrow arr a b) (arr [a] [b])
(<$$>) = spawn

-- some skeletons

parEval :: (ParallelSpawn arr, NFData b) => arr (Parrow arr a b) (arr [a] [b])
parEval = (<$$>)

parZipWith :: (ParallelSpawn arr, ArrowApply arr, NFData c) => arr (arr (a, b) c, ([a], [b])) [c]
parZipWith = (second $ arr $ \(as, bs) ->  zipWith (,) as bs) >>> parMap

parMap :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (arr a b, [a]) [b]
parMap = (first $ arr repeat) >>> (first parEval) >>> app