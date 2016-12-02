{-
The MIT License (MIT)

Copyright (c) 2016 Martin Braun

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts #-}
module Parrows.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category hiding ((.))

import Control.Monad

import Data.Monoid
import Data.Maybe
import Data.List.Split
import Data.List

type NumCores = Int
type ChunkSize = Int

class Config a where
instance Config () where

class (Arrow arr, Config conf) => ArrowParallel arr a b conf where
    parEvalN :: conf -> [arr a b] -> arr [a] [b]

-- class that allows us to have syntactic sugar for parallelism
-- this is needed because our ArrowParallel class (and all the utilities listed below)
-- work with arrows instead of functions to combine the arrows.
-- but in some cases it might be more convenient to combine
-- the arrows with operators instead of using arrows

-- parallel versions of (***) and (&&&)

(|***|) :: (ArrowParallel arr a b (), ArrowParallel arr (Maybe a, Maybe c) (Maybe b, Maybe d) (), ArrowApply arr) => arr a b -> arr c d -> arr (a, c) (b, d)
(|***|) = parEval2 ()

(|&&&|) :: (ArrowParallel arr a b (), ArrowParallel arr (Maybe a, Maybe a) (Maybe b, Maybe c) (), ArrowApply arr) => arr a b -> arr a c -> arr a (b, c)
(|&&&|) f g = (arr $ \a -> (a, a)) >>> (f |***| g)

-- from http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr f = arr listcase >>>
         arr (const []) ||| (f *** mapArr f >>> arr (uncurry (:)))
         where listcase [] = Left ()
               listcase (x:xs) = Right (x,xs)

zipWithArr :: ArrowChoice arr => arr (a, b) c -> arr ([a], [b]) [c]
zipWithArr zipFn = (arr $ \(as, bs) -> zipWith (,) as bs) >>> mapArr zipFn

listApp :: (ArrowChoice arr, ArrowApply arr) => arr [(arr a b, a)] [b]
listApp = (arr $ \fn -> (mapArr app, fn)) >>> app

listsApp :: (ArrowChoice arr, ArrowApply arr) => arr ([arr a b], [a]) [b]
listsApp = (arr $ \(fs, as) -> zipWith (,) fs as) >>> listApp

-- some really basic sugar

(...) :: (Arrow arr) => [arr a b] -> arr b c -> [arr a c]
(...) parr arr = map (>>> arr) parr

(....) :: (Arrow arr) => [arr a b] -> [arr b c] -> [arr a c]
(....) f g = zipWith (>>>) f g

-- behaves like <*> on lists and combines them with (....)
(<*....>) :: (Arrow arr) => [arr a b] -> [arr b c] -> [arr a c]
(<*....>) fs gs = concat $ zipWith (....) (repeat fs) (permutations gs)

-- minor stuff, remove this? these are basically just operations on lists

toPar :: (Arrow arr) => arr a b -> [arr a b]
toPar = return
(<|||>) :: (Arrow arr) => [arr a b] -> arr a b -> [arr a b]
(<|||>) fs g = fs ++ [g]
(<||||>) :: (Arrow arr) => [arr a b] -> [arr a b] -> [arr a b]
(<||||>) = (++)

-- spawns the first n arrows to be evaluated in parallel. this works for infinite lists of arrows as well
parEvalNLazy :: (Config conf, ArrowParallel arr a b conf, ArrowChoice arr, ArrowApply arr) => conf -> [arr a b] -> ChunkSize -> (arr [a] [b])
parEvalNLazy conf fs chunkSize =
               -- evaluate the function chunks in parallel and concat the input to a single list again
               (arr $ \as -> zipWith (,) fchunks (chunksOf chunkSize as)) >>> listApp >>> (arr $ concat)
               where
                -- chunk the functions, feed the function chunks into parEvalN, chunk the input accordingly
                fchunks = map (\x -> parEvalN conf x) $ chunksOf chunkSize fs

-- evaluate two functions with different types in parallel
parEval2 :: (Config conf, ArrowParallel arr a b conf, ArrowParallel arr (Maybe a, Maybe c) (Maybe b, Maybe d) conf, ArrowApply arr) => conf -> arr a b -> arr c d -> (arr (a, c) (b, d))
parEval2 conf f g = -- lift the functions to "maybe evaluated" functions
           -- so that if they are passed a Nothing they don't compute anything
           -- then, make a list of two of these functions evaluated after each other,
           -- feed each function the real value and one Nothing for the function they don't have to compute
           -- and combine them back to a tuple
           (arr $ \(a, c) -> (f_g, [(Just a, Nothing), (Nothing, Just c)])) >>> app >>> (arr $ \comb -> (fromJust (fst (comb !! 0)), fromJust (snd (comb !! 1))))
           where
               f_g = parEvalN conf $ replicate 2 $ arrMaybe f *** arrMaybe g
               arrMaybe :: (ArrowApply arr) => (arr a b) -> arr (Maybe a) (Maybe b)
               arrMaybe fn = (arr $ go) >>> app
                   where go Nothing = (arr $ \Nothing -> Nothing, Nothing)
                         go (Just a) = ((arr $ \(Just x) -> (fn, x)) >>> app >>> arr Just, (Just a))
						 
-- some skeletons

parMap :: (Config conf, ArrowParallel arr a b conf, ArrowApply arr) => conf -> (arr a b) -> (arr [a] [b])
parMap conf f = (arr $ \as -> (parEvalN conf (repeat f), as)) >>> app

parMapStream :: (Config conf, ArrowParallel arr a b conf, ArrowChoice arr, ArrowApply arr) => conf -> arr a b -> ChunkSize -> (arr [a] [b])
parMapStream conf f chunkSize = (arr $ \as -> (parEvalNLazy conf (repeat f) chunkSize, as)) >>> app

farmChunk :: (Config conf, ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, ArrowChoice arr, ArrowApply arr) => conf -> arr a b -> ChunkSize -> NumCores -> (arr [a] [b])
farmChunk conf f chunkSize numCores = -- chunk the input, inside of a chunk, behave sequentially,
                                 -- transform the map-chunks into a parallel function and apply it, then concat them together
                                 (arr $ \as -> (parEvalNLazy conf (repeat (mapArr f)) chunkSize, unshuffle numCores as)) >>> app >>> (arr shuffle)

-- contrary to parMap this schedules chunks of a given size (parMap has "chunks" of length = 1) to be
-- evaluated on the same thread
farm :: (Config conf, ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, ArrowChoice arr, ArrowApply arr) => conf -> arr a b -> NumCores -> (arr [a] [b])
farm conf f numCores =  -- chunk the input
                (arr $ \as -> (f, unshuffle numCores as)) >>>
                -- inside of a chunk, behave sequentially, transform the map-chunks into a parallel function and apply it
                (first $ arr mapArr) >>> (first $ (arr $ \f -> parEvalN conf (repeat f))) >>> app >>>
                -- [[b]] --> [b]
                (arr shuffle)
				
-- okay. (from: https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/src/Control-Parallel-Eden-Auxiliary.html#unshuffle)
unshuffle :: Int      -- ^number of sublists
             -> [a]   -- ^input list
             -> [[a]] -- ^distributed output
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]

takeEach :: Int -> [a] -> [a] 
takeEach n [] = []
takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)


-- | Simple shuffling - inverse to round robin distribution
shuffle :: [[a]]  -- ^ sublists
           -> [a] -- ^ shuffled sublists
shuffle = concat . transpose