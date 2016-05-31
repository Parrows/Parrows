module ParallelSplit.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category

import Control.Monad

import Data.Monoid
import Data.List.Split
import Data.List

type Parrow arr a b = [arr a b]

class (Arrow arr) => ParallelSpawn arr where
    parEvalN :: (NFData b) => arr (Parrow arr a b) (arr [a] [b])

-- is this needed?

class (Monad m) => MonadUnwrap m where
    unwrap :: m a -> a

-- is this needed? probably because of the same reason as MonadUnwrap is needed
-- in order to lift an arrow to a map we need to know about the internals (see Kleisli))
-- also this is the same requirement that ParallelSplit needs to be a class as we cannot

class (Arrow arr) => MappableArrow arr where
    toMap :: arr (arr a b) (arr [a] [b])

instance MappableArrow (->) where
    toMap f = \as -> map f as

instance (MonadUnwrap m) => MappableArrow (Kleisli m) where
    toMap = Kleisli $ \(Kleisli f) -> return (Kleisli $ \as -> return (map (\a -> unwrap $ f a) as))

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

-- evaluate two functions with different types in parallel

--thing :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr [arr [a] [b]] (arr [[a]] [[b]])
--thing = (arr $ \fchunks -> ((arr $ zipWith (,) fchunks) >>> _))

--parEvalNLazy :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (Parrow arr a b, Int) (arr [a] [b])
--parEvalNLazy = (arr $ \(fs, chunkSize) -> (chunksOf chunkSize fs, chunkSize)) >>>
--               (first $ (arr $ map (\x -> arr $ parEvalN x))) >>>
--               (second $ (arr $ \chunkSize -> (arr $ chunksOf chunkSize))) >>>
--               (arr $ \(fchunks, achunkfn) -> (arr $ \as -> (achunkfn, as)) >>> app >>> _)


--               (arr $ \(fchunks, chunkSize) -> arr ($)) >>> _

parEval2 :: (ParallelSpawn arr, ArrowApply arr, NFData b, NFData d) => arr (arr a b, arr c d) (arr (a, c) (b, d))
parEval2 = (arr $ \(f, g) -> (arrMaybe f, arrMaybe g)) >>>
         (arr $ \(f, g) -> replicate 2 (first f >>> second g)) >>> parEvalN >>>
         (arr $ \f -> (arr $ \(a, c) -> (f, [(Just a, Nothing), (Nothing, Just c)])) >>> app >>> (arr $ \comb -> (uwrap (fst (comb !! 0)), uwrap (snd (comb !! 1)))))
         where
             uwrap (Just x) = x
             uwrap (Nothing) = error "unexpected Nothing"
             arrMaybe :: (ArrowApply arr) => (arr a b) -> arr (Maybe a) (Maybe b)
             arrMaybe fn = (arr $ go) >>> app
                 where go Nothing = (arr $ \Nothing -> Nothing, Nothing)
                       go (Just a) = ((arr $ \(Just x) -> (fn, x)) >>> app >>> arr Just, (Just a))

-- some skeletons

parZipWith :: (ParallelSpawn arr, ArrowApply arr, NFData c) => arr (arr (a, b) c, ([a], [b])) [c]
parZipWith = (second $ arr $ \(as, bs) ->  zipWith (,) as bs) >>> parMap

parMap :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (arr a b, [a]) [b]
parMap = (first $ arr repeat) >>> (first parEvalN) >>> app