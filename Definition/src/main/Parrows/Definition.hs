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
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parrows.Definition where

import           Parrows.Util

import           Control.Arrow

import           Data.Either
import           Data.List.Split

--infixr 3 `|***|`
--infixr 3 `|&&&|`
--infixr 3 `|>>>|`

type NumCores = Int
type ChunkSize = Int

class (Arrow arr) => ArrowParallel arr a b conf where
    parEvalN :: conf -> [arr a b] -> arr [a] [b]

-- parallel versions of (***) and (&&&)

(|***|) :: (ArrowChoice arr, ArrowParallel arr (Either a c) (Either b d) ()) => arr a b -> arr c d -> arr (a, c) (b, d)
(|***|) = parEval2 ()

(|&&&|) :: (ArrowChoice arr, ArrowParallel arr (Either a a) (Either b c) ()) => arr a b -> arr a c -> arr a (b, c)
(|&&&|) f g = arr (\a -> (a, a)) >>> f |***| g

-- some really basic sugar

(...) :: (Arrow arr) => [arr a b] -> arr b c -> [arr a c]
(...) parr f = map (>>> f) parr

-- spawns the first n arrows to be evaluated in parallel. this works for infinite lists of arrows as well
parEvalNLazy :: (ArrowParallel arr a b conf, ArrowChoice arr) => conf  -> ChunkSize -> [arr a b] -> arr [a] [b]
parEvalNLazy conf chunkSize fs =
               -- chunk the functions, feed the function chunks into parEvalN, chunk the input accordingly
               -- evaluate the function chunks in parallel and concat the input to a single list again
               arr (chunksOf chunkSize) >>>
               evalN fchunks >>>
               arr concat
               where
                fchunks = map (parEvalN conf) $ chunksOf chunkSize fs

-- evaluate two functions with different types in parallel
parEval2 :: (ArrowChoice arr, ArrowParallel arr (Either a c) (Either b d) conf) => conf -> arr a b -> arr c d -> arr (a, c) (b, d)
parEval2 conf f g =
           arr Left *** (arr Right >>> arr return) >>>
           arr (uncurry (:)) >>>
           parEvalN conf (replicate 2 (f +++ g)) >>>
           arr partitionEithers >>>
           arr head *** arr head
