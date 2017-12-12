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
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Parrows.Eden(
  Conf,
  defaultConf,
  module Parrows.Definition,
  module Parrows.Future
) where

import           Parrows.Definition
import           Parrows.Future
import           Parrows.Util

import           Control.Arrow

import           Control.Parallel.Eden

data RemoteData a = RD { rd :: RD a }
instance NFData (RemoteData a) where
    rnf = rnf . rd
instance Trans (RemoteData a)

data Conf a = Nil

defaultConf :: [arr a b] -> Conf b
defaultConf _ = Nil

instance (Trans a) => Future RemoteData a where
    put = arr (\a -> RD { rd = release a })
    get = arr rd >>> arr fetch

instance (ArrowChoice arr, ArrowParallel arr a b (Conf b)) => FutureEval arr a b (Conf b) where
    headStrictEvalN = parEvalN
    postHeadStrictEvalN _ = listApp

instance (Trans a, Trans b) => ArrowParallel (->) a b (Conf b) where
    parEvalN _ = spawnF

instance (ArrowParallel (->) a (m b) (Conf b), Monad m, Trans a, Trans b, Trans (m b)) => ArrowParallel (Kleisli m) a b (Conf b) where
    parEvalN conf fs = arr (parEvalN conf (map (\(Kleisli f) -> f) fs)) >>>
                       Kleisli sequence
