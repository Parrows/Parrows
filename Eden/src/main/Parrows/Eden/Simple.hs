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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parrows.Eden.Simple where

import Parrows.Eden
import Control.Arrow
import Control.Parallel.Eden (Trans)

instance (Trans b, ArrowChoice arr, ArrowParallel arr a b Conf) => ArrowParallel arr a b () where
    parEvalN _ fs = parEvalN defaultConf fs

instance (Trans b, ArrowChoice arr, ArrowParallel arr a b (), FutureEval arr a b Conf) => FutureEval arr a b () where
    headStrictEvalN _ fs = headStrictEvalN defaultConf fs
    postHeadStrictEvalN _  fs = postHeadStrictEvalN defaultConf fs

instance (Trans a) => Future RemoteData a () where
    put _ = put'
    get _ = get'
