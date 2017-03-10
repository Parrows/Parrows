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
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Parrows.Skeletons.Map where

import Control.Arrow

import Parrows.Definition
import Parrows.Util

import Data.List.Split

-- some map skeletons

parMap :: (ArrowParallel arr a b conf, ArrowApply arr) => conf -> (arr a b) -> (arr [a] [b])
parMap conf f = (arr $ \as -> (f, as)) >>>
                (first $ arr repeat >>> arr (parEvalN conf)) >>>
                app

-- contrary to parMap this schedules chunks of a given size (parMap has "chunks" of length = 1) to be
-- evaluated on the same thread
parMapStream :: (ArrowParallel arr a b conf, ArrowChoice arr, ArrowApply arr) => conf -> ChunkSize -> arr a b -> arr [a] [b]
parMapStream conf chunkSize f = (arr $ \as -> (f, as)) >>>
                                (first $ arr repeat >>> arr (parEvalNLazy conf chunkSize)) >>>
                                app

-- similar to parMapStream, but divides the input list by the given number
farm :: (ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, ArrowChoice arr, ArrowApply arr) => conf -> NumCores -> arr a b -> arr [a] [b]
farm conf numCores f =
                (arr $ \as -> (f, as)) >>>
                (first $ arr mapArr >>> arr repeat >>> arr (parEvalN conf)) >>>
                (second $ arr (unshuffle numCores)) >>>
                app >>>
                arr shuffle

-- farmChunk and parMapStream combined. divide the input list and inside work in chunks
farmChunk :: (ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, ArrowChoice arr, ArrowApply arr) => conf -> ChunkSize -> NumCores -> arr a b -> arr [a] [b]
farmChunk conf chunkSize numCores f =
                                 (arr $ \as -> (f, as)) >>>
                                 (first $ arr mapArr >>> arr repeat >>> arr (parEvalNLazy conf chunkSize)) >>>
                                 (second $ arr (unshuffle numCores)) >>>
                                 app >>>
                                 arr shuffle

-- this does not completely adhere to Google's definition of Map Reduce as it
-- the mapping function does not allow for "reordering" of the output
-- The original Google version can be found at https://de.wikipedia.org/wiki/MapReduce
parMapReduceDirect :: (ArrowParallel arr [a] b conf, ArrowApply arr, ArrowChoice arr) => conf -> ChunkSize -> arr a b -> arr (b, b) b -> b -> arr [a] b
parMapReduceDirect conf chunkSize mapfn foldfn neutral =
                                   (arr $ chunksOf chunkSize) >>>
                                   parMap conf (mapArr mapfn >>> foldlArr foldfn neutral) >>>
                                   (foldlArr foldfn neutral)
