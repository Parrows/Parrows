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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Parrows.ParMonad where

import           Parrows.Definition
import           Parrows.Future
import           Parrows.Util

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad.Par

instance (NFData b, ArrowChoice arr) => ArrowParallel arr a b conf where
    parEvalN _ fs = listApp (map (>>> arr spawnP) fs) >>>
                    arr sequenceA >>>
                    arr (>>= mapM Control.Monad.Par.get) >>>
                    arr runPar

data BasicFuture a = BF a

instance NFData a => NFData (BasicFuture a) where
    rnf (BF a) = rnf a

instance (ArrowChoice arr, ArrowParallel arr a b conf) => FutureEval arr a b conf where
    distributedEvalN _ = listApp
    sharedEvalN = parEvalN

instance Future BasicFuture a where
    put = arr BF
    get = arr (\(~(BF a)) -> a)
