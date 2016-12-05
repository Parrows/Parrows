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
module Parrows.HdpH where

import Parrows.Definition
import Control.Arrow

import Data.Maybe

import Control.Applicative
import Control.Monad

import Control.DeepSeq
import Control.Parallel.HdpH
import Control.Parallel.HdpH.Strategies

import System.IO.Unsafe (unsafePerformIO)

-- TODO: check whether it is okay that we spawn "exponentially"

-- class to extract the RTSConf out of conf
class HdpHConf conf where
    rtsConf :: conf -> RTSConf

-- obvious default instance
instance HdpHConf RTSConf where
    rtsConf = id

-- class to extract the Strategy out of the conf
class HdpHStrategy conf b where
    strategy :: conf -> Strategy [Closure b]

-- default implementation for basic RTSConf, uses (parClosureList forceCC) as the strategy
instance (ForceCC b) => HdpHStrategy RTSConf b where
    strategy _ = parClosureList forceCC

instance (ToClosure b, HdpHConf conf, HdpHStrategy conf b, ArrowApply arr, ArrowChoice arr) => ArrowParallel arr a b conf where
    parEvalN conf fs = (arr $ zipWith (,) fs) >>> listApp >>>
                        (arr $ map toClosure) >>>
                        (arr $ flip using $ strategy conf) >>>
                        (arr $ runParIO $ rtsConf conf) >>>
                        (arr $ unsafePerformIO) >>>
                        (arr $ fromJust') >>>
                        (arr $ map unClosure)
        where fromJust' :: Maybe [b] -> [b]
              -- just to make sure that this doesn't throw an error
              fromJust' Nothing = []
              fromJust' bs = fromJust bs