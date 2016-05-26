{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ParallelSplit.Definition where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List.Split
import Control.Category
import Control.Arrow
import Data.Monoid

data Parrow arr a b = Parrow [arr a b]

instance Monoid (Parrow arr a b) where
    mempty = Parrow []
    mappend (Parrow xs) (Parrow ys) = Parrow $ xs `mappend` ys

-- TODO: is Parrow a monad?

toPar :: (Arrow arr) => arr a b -> Parrow arr a b
toPar arr = Parrow $ [arr]
(<|||>) :: (Arrow arr) => Parrow arr a b -> arr a b -> Parrow arr a b
(<|||>) (Parrow fs) g = Parrow $ fs `mappend` [g]
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

-- this could be broken down for arrows so we can use this kind
-- of parallelism with normal functions
class (Arrow arr) => ParallelSpawn arr where
    spawn :: (NFData b) => Parrow arr a b -> arr [a] [b]

-- default implementation without threading
--instance (ArrowRun arr) => ParallelSpawn arr where
--    spawn (Parrow fs) = arr $ zipWith fs