{-
The MIT License (MIT)

Copyright (c) 2016-2017 Martin Braun

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
module Parrows.Dummy(
  Strategy,
  Conf(..),
  defaultConf,
  stratToConf,
  module Parrows.Definition,
  module Parrows.Future,
  module Control.DeepSeq
) where

import           Parrows.Definition
import           Parrows.Future
import           Parrows.Util

import           Control.Arrow
import           Control.DeepSeq

type Strategy a = a -> a

data Conf a = Conf (Strategy a)

defaultConf :: (NFData b) => [arr a b] -> Conf b
defaultConf fs = stratToConf fs force

stratToConf :: [arr a b] -> Strategy b -> Conf b
stratToConf _ strat = Conf strat

instance (ArrowChoice arr) => ArrowParallel arr a b (Conf b) where
    parEvalN (Conf strat) fs = evalN (map (>>> arr strat) fs)

instance (ArrowChoice arr, ArrowParallel arr a b (Conf b)) => ArrowLoopParallel arr a b (Conf b) where
    loopParEvalN _ = evalN
    postLoopParEvalN = parEvalN

instance Future BasicFuture a (Conf a) where
    put _ = put'
    get _ = get'
