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
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module Parrows.Eden where

import Parrows.Definition
import Parrows.Future
import Parrows.Util

import Control.Arrow

import Control.Parallel.Eden

data RemoteData a = RD { rd :: RD a }
instance NFData (RemoteData a) where
    rnf = rnf . rd
instance Trans (RemoteData a)

instance (Trans a) => Future RemoteData a where
    put = arr (\a -> RD { rd = release a })
    get = arr rd >>> arr fetch

instance (ArrowParallel arr a b conf) => FutureEval arr where
    evalN = parEvalN

-- ArrowParallel Instances

-- FIXME: will this work with (spawnF id bs) with already "computed" bs
-- so that we can write a uniform instance that doesn't require
-- the weird unwrapping of the Kleisli type?
-- Probably not, because we would end up computing the values while
-- sending them over the network, right?

--instance (Trans a) => Trans (Lazy a)
--instance (NFData a) => NFData (Lazy a) where
--    rnf x = rnf $ unLazy x


instance (Trans a, Trans b) => ArrowParallel (->) a b conf where
    parEvalN _ fs as = spawnF fs as

instance (Monad m, Trans a, Trans b, Trans (m b)) => ArrowParallel (Kleisli m) a b conf where
    parEvalN conf fs = (arr $ parEvalN conf (map (\(Kleisli f) -> f) fs)) >>> (Kleisli $ sequence)
