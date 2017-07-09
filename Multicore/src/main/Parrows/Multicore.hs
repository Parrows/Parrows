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
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Parrows.Multicore where

import Parrows.Definition
import Parrows.Future
import Parrows.Util

import Control.Parallel
import Control.Parallel.Strategies
import Control.Arrow
import Control.DeepSeq

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe

data Conf a = Conf (Strategy a)

instance (NFData b, ArrowApply arr, ArrowChoice arr) => ArrowParallel arr a b (Conf b) where
    parEvalN (Conf strat) fs =
        listApp fs >>>
        arr (withStrategy (parList strat)) &&& arr id >>>
        arr (uncurry pseq)

instance (NFData b, ArrowApply arr, ArrowChoice arr) => ArrowParallel arr a b () where
    parEvalN _ fs = parEvalN (hack fs) fs
                    where
                        hack :: (NFData b) => [arr a b] -> Conf b
                        hack _ = Conf rdeepseq

data BasicFuture a = BF a

instance (NFData a) => NFData (BasicFuture a) where
    rnf (BF a) = rnf a

instance (NFData a) => Future BasicFuture a where
    put = arr BF
    get = arr $ ((BF a) -> a)