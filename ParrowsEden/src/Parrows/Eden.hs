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
{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, Rank2Types, MultiParamTypeClasses #-}
module Parrows.Eden where

import Parrows.Definition

import Control.Parallel.Strategies
import Control.Arrow

import Control.Parallel.Eden

class EdenArrow arr a b where
    spawnArrow :: arr ([arr a b], [a]) [b]

instance ParallelSpawn (->) where
    parEvalN fs as = spawn (map process fs) as

instance (Trans a, Trans b) => EdenArrow (->) a b where
    spawnArrow (fs, as) = spawn (map process fs) as

instance (Trans a, Trans b, Monad m, Trans (m b)) => EdenArrow (Kleisli m) a b where
    spawnArrow = Kleisli $ \(fs,as) -> sequence (spawn (map (\(Kleisli f) -> process f) fs) as)


--instance ParallelSpawn (Kleisli m) where
--    parEvalN = Kleisli $ \fs -> ((Kleisli $ \as -> (spawn (map process (map (\(Kleisli f) -> f) fs)) as)))

--instance (ArrowApply arr, ArrowChoice arr) => ParallelSpawn arr where
--    parEvalN = arr $ \fs -> ((arr $ \as -> zipWith (,) fs as) >>> listApp >>> (arr $ \bs -> bs `using` parList rdeepseq))