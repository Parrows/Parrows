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
module Parrows.ParMonad(
  Strategy,
  Conf(..),
  defaultConf,
  stratToConf,
  strict,
  headStrict,
  module Parrows.Definition,
  module Parrows.Future,
  module Control.DeepSeq
) where

import           Parrows.Definition
import           Parrows.Future
import           Parrows.Util

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad.Par

type Strategy a = a -> Par (IVar a)

strict :: (NFData a) => Strategy a
strict = spawn . return

headStrict :: Strategy a
headStrict = spawn_ . return

data Conf a = Conf (Strategy a)

defaultConf :: (NFData b) => [arr a b] -> Conf b
defaultConf fs = stratToConf fs strict

stratToConf :: [arr a b] -> Strategy b -> Conf b
stratToConf _ strat = Conf strat

instance (NFData b, ArrowChoice arr) => ArrowParallel arr a b (Conf b) where
    parEvalN (Conf strat) fs = evalN (map (>>> arr strat) fs) >>>
                    arr sequenceA >>>
                    arr (>>= mapM Control.Monad.Par.get) >>>
                    arr runPar

instance (ArrowChoice arr, ArrowParallel arr a b (Conf b)) => ArrowLoopParallel arr a b (Conf b) where
    loopParEvalN _ fs = parEvalN (stratToConf fs headStrict) fs
    postLoopParEvalN = parEvalN

instance Future BasicFuture a (Conf a) where
    put _ = put'
    get _ = get'
