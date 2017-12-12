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
import           Control.DeepSeq
import           Parrows.Definition

class Future fut a conf | a conf -> fut where
    put :: (Arrow arr) => conf -> arr a (fut a)
    get :: (Arrow arr) => conf -> arr (fut a) a

class ArrowParallel arr a b conf => FutureEval arr a b conf where
    headStrictEvalN :: conf -> [arr a b] -> arr [a] [b]
    postHeadStrictEvalN :: conf -> [arr a b] -> arr [a] [b]

data BasicFuture a = BF a

put' :: (Arrow arr) => arr a (BasicFuture a)
put' = arr BF

get' :: (Arrow arr) => arr (BasicFuture a) a
get' = arr (\(~(BF a)) -> a)

instance NFData a => NFData (BasicFuture a) where
    rnf (BF a) = rnf a

{-
get :: (Arrow arr, Future fut a conf) => conf -> arr a (fut a)
get conf = get' (typeToken [([]::a, [conf])])

put :: (Arrow arr, Future fut a conf) => conf -> arr (fut a) a
put conf = put' (typeToken _)
-}

liftFut :: (Arrow arr, Future fut a conf, Future fut b conf) => conf -> arr a b -> arr (fut a) (fut b)
liftFut conf f = get conf >>> f >>> put conf

unliftFut :: (Arrow arr, Future fut a conf, Future fut b conf) => conf -> arr (fut a) (fut b) -> arr a b
unliftFut conf f = put conf >>> f >>> get conf

parEvalNFut :: (ArrowParallel arr (fut a) (fut b) conf, Future fut a conf, Future fut b conf) => conf -> [arr a b] -> arr [fut a] [fut b]
parEvalNFut conf fs = parEvalN conf $ map (liftFut conf) fs
