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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parrows.Dummy.Simple where

import Parrows.Dummy
import Control.Arrow

instance (NFData b, ArrowChoice arr, ArrowParallel arr a b (Conf b)) => ArrowParallel arr a b () where
    parEvalN _ fs = parEvalN (defaultConf fs) fs

instance (NFData b, ArrowChoice arr, ArrowParallel arr a b (), ArrowLoopParallel arr a b (Conf b)) => ArrowLoopParallel arr a b () where
    loopParEvalN _ fs = loopParEvalN (defaultConf fs) fs
    postLoopParEvalN _  fs = postLoopParEvalN (defaultConf fs) fs

instance Future BasicFuture b () where
    put _ = put'
    get _ = get'
