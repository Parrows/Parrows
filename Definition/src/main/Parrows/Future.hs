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
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Parrows.Future where

import           Control.Arrow
import           Parrows.Definition

class Future fut a | a -> fut where
    put :: (Arrow arr) => arr a (fut a)
    get :: (Arrow arr) => arr (fut a) a

class ArrowParallel arr a b conf => FutureEval arr a b conf where
    evalN :: (ArrowParallel arr a b conf) => conf -> [arr a b] -> arr [a] [b]

liftFut :: (Arrow arr, Future fut a, Future fut b) => arr a b -> arr (fut a) (fut b)
liftFut f = get >>> f >>> put

unliftFut :: (Arrow arr, Future fut a, Future fut b) => arr (fut a) (fut b) -> arr a b
unliftFut f = put >>> f >>> get

parEvalNFut :: (ArrowParallel arr (fut a) (fut b) conf, Future fut a, Future fut b) => conf -> [arr a b] -> arr [fut a] [fut b]
parEvalNFut conf fs = parEvalN conf $ map liftFut fs
