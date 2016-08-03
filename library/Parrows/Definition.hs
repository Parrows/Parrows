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
module Parrows.Definition where

import Control.Arrow
import Control.DeepSeq
import Control.Category

import Control.Monad

import Data.Monoid
import Data.Maybe
import Data.List.Split
import Data.List

type Parrow arr a b = [arr a b]

class (Arrow arr) => ParallelSpawn arr where
    parEvalN :: (NFData b) => arr (Parrow arr a b) (arr [a] [b])

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

-- some sugar

(...) :: (Arrow arr) => Parrow arr a b -> arr b c -> Parrow arr a c
(...) parr arr = map (>>> arr) parr

(....) :: (Arrow arr) => Parrow arr a b -> Parrow arr b c -> Parrow arr a c
(....) f g = zipWith (>>>) f g

-- takes a and produces and arrow that maps from b to (a, b)
-- utility function for usage of parMap etc.
-- someArrow >>> (tup $ arrow) >>> parMap
tup :: (Arrow arr) => a -> arr b (a, b)
tup a = arr $ \b -> (a, b)

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

-- spawns the first n arrows to be evaluated in parallel. this works for infinite lists
-- of arrows as well (think: parEvalNLazy (map (*) [1..], 10) [2..])
parEvalNLazy :: (ParallelSpawn arr, ArrowChoice arr, ArrowApply arr, NFData b) => arr (Parrow arr a b, Int) (arr [a] [b])
parEvalNLazy = -- chunk the functions
               (arr $ \(fs, chunkSize) -> (chunksOf chunkSize fs, chunkSize)) >>>
               -- feed the function chunks into parEvalN
               (first $ (arr $ map (\x -> (parEvalN, x))) >>> listApp) >>>
               -- chunk the input accordingly
               (second $ (arr $ \chunkSize -> (arr $ chunksOf chunkSize))) >>>
               -- evaluate the function chunks in parallel and concat the input to a single list again
               (arr $ \(fchunks, achunkfn) -> (arr $ \as -> (achunkfn, as)) >>> app >>> (arr $ zipWith (,) fchunks) >>> listApp >>> (arr $ concat) )

-- evaluate two functions with different types in parallel
parEval2 :: (ParallelSpawn arr, ArrowApply arr, NFData b, NFData d) => arr (arr a b, arr c d) (arr (a, c) (b, d))
parEval2 = -- lift the functions to "maybe evaluated" functions
           -- so that if they are passed a Nothing they don't compute anything
           (arr $ \(f, g) -> (arrMaybe f, arrMaybe g)) >>>
           -- make a list of two of these functions evaluated after each other
           (arr $ \(f, g) -> replicate 2 (first f >>> second g)) >>> parEvalN >>>
           -- feed each function the real value and one Nothing for the function they don't have to compute
           -- and combine them back to a tuple
           (arr $ \f -> (arr $ \(a, c) -> (f, [(Just a, Nothing), (Nothing, Just c)])) >>> app >>> (arr $ \comb -> (fromJust (fst (comb !! 0)), fromJust (snd (comb !! 1)))))
           where
               arrMaybe :: (ArrowApply arr) => (arr a b) -> arr (Maybe a) (Maybe b)
               arrMaybe fn = (arr $ go) >>> app
                   where go Nothing = (arr $ \Nothing -> Nothing, Nothing)
                         go (Just a) = ((arr $ \(Just x) -> (fn, x)) >>> app >>> arr Just, (Just a))

-- some skeletons

parZipWith :: (ParallelSpawn arr, ArrowApply arr, NFData c) => arr (arr (a, b) c, ([a], [b])) [c]
parZipWith = (second $ arr $ \(as, bs) ->  zipWith (,) as bs) >>> parMap

-- same as parZipWith but with the same difference as parMapChunky has compared to parMap
parZipWithChunky :: (ParallelSpawn arr, ArrowChoice arr, ArrowApply arr, NFData c) => arr (arr (a, b) c, ([a], [b], Int)) [c]
parZipWithChunky = (second $ arr $ \(as, bs, chunkSize) -> (zipWith (,) as bs, chunkSize)) >>> parMapChunky

parMap :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (arr a b, [a]) [b]
parMap = (first $ arr repeat) >>> (first parEvalN) >>> app

-- contrary to parMap this schedules chunks of a given size (parMap has "chunks" of length = 1) to be
-- evaluated on the same thread
parMapChunky :: (ParallelSpawn arr, ArrowChoice arr, ArrowApply arr, NFData b) => arr (arr a b, ([a], Int)) [b]
parMapChunky =  -- chunk the input
                (second $ arr $ \(as, chunkSize) -> (chunksOf chunkSize as)) >>>
                -- inside of a chunk, behave sequentially
                (first $ arr mapArr) >>> (first $ arr repeat) >>>
                -- transform the map-chunks into a parallel function and apply it
                (first $ parEvalN) >>> app >>>
                -- [[b]] --> [b]
                (arr concat)