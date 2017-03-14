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

import Parrows.Util

import Control.Arrow
import Control.DeepSeq

import Control.Monad

import Data.Monoid
import Data.Maybe
import Data.List.Split
import Data.List

--infixr 3 `|***|`
--infixr 3 `|&&&|`
--infixr 3 `|>>>|`

type NumCores = Int
type ChunkSize = Int

class Arrow arr => ArrowParallel arr a b conf where
    parEvalN :: conf -> [arr a b] -> arr [a] [b]

-- parallel versions of (***) and (&&&)

(|***|) :: (ArrowParallel arr a b (), ArrowParallel arr (Maybe a, Maybe c) (Maybe b, Maybe d) (), ArrowApply arr) => arr a b -> arr c d -> arr (a, c) (b, d)
(|***|) = parEval2 ()

(|&&&|) :: (ArrowParallel arr a b (), ArrowParallel arr (Maybe a, Maybe a) (Maybe b, Maybe c) (), ArrowApply arr) => arr a b -> arr a c -> arr a (b, c)
(|&&&|) f g = (arr $ \a -> (a, a)) >>> f |***| g

(|>>>|) :: (Arrow arr) => [arr a b] -> [arr b c] -> [arr a c]
(|>>>|) = zipWith (>>>)

-- some really basic sugar

(...) :: (Arrow arr) => [arr a b] -> arr b c -> [arr a c]
(...) parr arr = map (>>> arr) parr

-- behaves like <*> on lists and combines them with (|>>>|)
(|<*>|) :: (Arrow arr) => [arr a b] -> [arr b c] -> [arr a c]
(|<*>|) fs gs = concat $ zipWith (|>>>|) (repeat fs) (permutations gs)

-- minor stuff, remove this? these are basically just operations on lists

toPar :: (Arrow arr) => arr a b -> [arr a b]
toPar = return
(<|||>) :: (Arrow arr) => [arr a b] -> arr a b -> [arr a b]
(<|||>) fs g = fs ++ [g]
(<||||>) :: (Arrow arr) => [arr a b] -> [arr a b] -> [arr a b]
(<||||>) = (++)

-- spawns the first n arrows to be evaluated in parallel. this works for infinite lists of arrows as well
parEvalNLazy :: (ArrowParallel arr a b conf, ArrowChoice arr, ArrowApply arr) => conf  -> ChunkSize -> [arr a b] -> (arr [a] [b])
parEvalNLazy conf chunkSize fs =
               -- chunk the functions, feed the function chunks into parEvalN, chunk the input accordingly
               -- evaluate the function chunks in parallel and concat the input to a single list again
               arr (chunksOf chunkSize) >>>
               listApp fchunks >>>
               arr concat
               where
                fchunks = map (parEvalN conf) $ chunksOf chunkSize fs

-- evaluate two functions with different types in parallel
parEval2 :: (ArrowParallel arr a b conf, ArrowParallel arr (Maybe a, Maybe c) (Maybe b, Maybe d) conf, ArrowApply arr) => conf -> arr a b -> arr c d -> (arr (a, c) (b, d))
parEval2 conf f g = -- lift the functions to "maybe evaluated" functions
           -- so that if they are passed a Nothing they don't compute anything
           -- then, make a list of two of these functions evaluated after each other,
           -- feed each function the real value and one Nothing for the function they don't have to compute
           -- and combine them back to a tuple
           (arr $ \(a, c) -> (f_g, [(Just a, Nothing), (Nothing, Just c)])) >>>
           app >>>
           (arr $ \comb -> (fromJust (fst (comb !! 0)), fromJust (snd (comb !! 1))))
           where
               f_g = parEvalN conf $ replicate 2 $ arrMaybe f *** arrMaybe g
               arrMaybe :: (ArrowApply arr) => (arr a b) -> arr (Maybe a) (Maybe b)
               arrMaybe fn = (arr $ go) >>> app
                   where go Nothing = (arr $ \Nothing -> Nothing, Nothing)
                         go (Just a) = ((arr $ \(Just x) -> (fn, x)) >>> app >>> arr Just, (Just a))

parEvalNM :: (ArrowChoice arr, ArrowApply arr, ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf) => conf -> [[arr a b]] -> arr [[a]] [[b]]
parEvalNM conf = listApp . (map (parEvalN conf))