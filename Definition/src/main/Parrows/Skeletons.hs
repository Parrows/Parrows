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
module Parrows.Skeletons where

import Control.Arrow

import Parrows.Definition
import Parrows.Future

parEvalNFut :: (ArrowParallel arr (fut a) (fut b) conf, Future fut a, Future fut b) => conf -> [arr a b] -> arr [fut a] [fut b]
parEvalNFut conf fs = parEvalN conf $ map liftFut fs

pipe :: (ArrowLoop arr, ArrowApply arr, ArrowParallel arr (fut a) (fut a) conf, Future fut a) => conf -> [arr a a] -> arr a a
pipe conf fs = unliftFut $ pipeFut conf fs

pipeFut :: (ArrowLoop arr, ArrowApply arr, ArrowParallel arr (fut a) (fut a) conf, Future fut a) => conf -> [arr a a] -> arr (fut a) (fut a)
pipeFut conf fs = resolve (arr $ \(a, outs) -> lazy $ a : outs) (parEvalNFut conf fs) >>> arr last
    where
        -- util for the infinite resolve fix place recursion with ArrowLoop(s)
        resolve :: (ArrowApply arr, ArrowLoop arr) => arr (a, b) c -> arr c b -> arr a b
        resolve transform f = loop $ (arr $ \(a, b) -> (b, (f, (transform, (a, b))))) >>> second (second app >>> app)

ring :: (ArrowLoop arr, ArrowApply arr, Future fut r, (ArrowParallel arr (i, fut r) (o, fut r) conf)) =>
    conf ->
    arr (i, r) (o, r) ->
    arr [i] [o]
ring conf f = loop $ second (arr rightRotate >>> arr lazy) >>> (arr $ uncurry zip) >>> (parMap conf (toFut $ f)) >>> arr unzip

toFut :: (Arrow arr, Future fut r) =>
        (arr (i, r) (o, r))              -- ^ ring process function
        -> (arr (i, fut r) (o, fut r))   -- ^ with remote data
toFut f = (second $ arr get) >>> f >>> (second $ arr put)

rightRotate    :: [a] -> [a]
rightRotate [] =  []
rightRotate xs =  last xs : init xs

-- From Eden:

-- | A lazy list is an infinite stream
lazy :: [a] -> [a]
lazy ~(x:xs) = x : lazy xs