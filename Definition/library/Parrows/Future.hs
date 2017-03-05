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
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, Rank2Types, FunctionalDependencies #-}
module Parrows.Future where

import Control.Arrow
import Parrows.Definition

class Future fut a | a -> fut where
    put :: a -> fut a
    get :: fut a -> a

liftFut :: (Arrow arr, Future fut a, Future fut b) => arr a b -> arr (fut a) (fut b)
liftFut f = (arr get) >>> f >>> (arr put)

parEvalNFut :: (ArrowParallel arr (fut a) (fut b) conf, Future fut a, Future fut b) => conf -> [arr a b] -> arr [fut a] [fut b]
parEvalNFut conf fs = parEvalN conf (map liftFut fs)

pipe :: (ArrowLoop arr, ArrowApply arr, ArrowParallel arr (fut a) (fut a) conf, Future fut a) => conf -> [arr a a] -> arr (fut a) (fut a)
pipe conf fs = (loop $ (arr $ \(a, outs) -> (outs, (parEvalNFut conf fs, lazy $ a : outs))) >>> (second $ app)) >>> arr last

-- From Eden:

-- | A lazy list is an infinite stream
lazy :: [a] -> [a]
lazy ~(x:xs) = x : lazy xs