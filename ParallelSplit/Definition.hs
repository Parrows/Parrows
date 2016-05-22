module ParallelSplit.Definition where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List.Split

-- basic arrows
class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  --first :: a b c -> a (b,d) (c,d)

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)

newtype Kleisli m a b = K (a -> m b)

instance Monad m => Arrow (Kleisli m) where
  arr f = K $ return . f
  K f >>> K g = K (\b -> f b >>= g)

-- now for parallel stuff
data ParRes a = PR a
                deriving Show

-- use the real Par monad or Eden's low-level interface or even Concurrent Haskell with Monad == IO here
-- maybe express it and ParKleisli in terms of Kleisli?

-- boilerplate begins
instance Monad ParRes where
    return = PR
    PR x >>= f = f x

instance Functor ParRes where
    fmap f (PR x) = PR $ f x

instance Applicative ParRes where
    -- PR (a -> b) <*> PR a
    PR f <*> PR x = PR (f x)
    pure = PR
-- boilerplate ends

instance (NFData a) => NFData (ParRes a) where
    rnf (PR a) = rnf a

newtype ParKleisli a b = P (a -> ParRes b)
-- same Kleisli type as above, so:
instance Arrow (ParKleisli) where
  arr f = P $ return . f
  P f >>> P g = P (\b -> f b >>= g)

-- using split <|||> and merge <&&&> basically every computation
-- can be parallelized but if the single tasks are already computed
-- quite fast, the overhead for splitting and merging
-- is probably too high (regarding both computationally and
-- code length/complexity due to the types ending up being something like
-- ((,),(,)), etc. if merges dont happen)

-- this could be broken down for arrows so we can use this kind
-- of parallelism with normal functions
class (Arrow arr) => ParallelSplit arr where
    (<|||=>) :: (NFData b) => arr a b -> arr a b -> arr [a] [b]
    (<|||==>) :: (NFData b) => arr a b -> arr [a] [b] -> arr [a] [b]
    (<&&&=>) :: arr [a] [b] -> (b -> b -> b) -> arr [a] b

    (<||>) :: (NFData b, NFData c) => arr a b -> arr a c -> arr a (b, c)
    (<&&>) :: arr a (b, c) -> (b -> c -> d) -> arr a d
    (<|||>) :: (NFData b, NFData d) => arr a b -> arr c d -> arr (a, c) (b, d)
    (<&&&>) :: arr (a, c) (b, d) -> (b -> d -> e) -> arr (a, c) e

chunkLen len threadcnt
   | threadcnt > len = 1
   | otherwise = len `div` threadcnt

chunky :: (ParallelSplit arr, NFData b) => Int -> (a -> b) -> arr [a] [b]
chunky 0 fn = arr $ \xs -> []
-- the last chunk evalutes the rest (if there is some)
-- this is by far better than the alternative of dropping
-- the values :D
chunky 1 fn = arr $ \xs -> map fn xs
chunky 2 fn = arr fn <|||=> arr fn
chunky x fn = arr fn <|||==> (chunky (x-1) fn)