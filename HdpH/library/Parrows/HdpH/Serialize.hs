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
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}
module Parrows.HdpH.Serialize where

import GHC.Packing
import Data.Binary
import Data.ByteString.Lazy
import Data.Typeable.Internal

import Parrows.HdpH.Closure

import Control.Parallel.HdpH.Closure

import System.IO.Unsafe (unsafePerformIO)

-- | default buffer size used by trySerialize
defaultBufSize :: Int
defaultBufSize = 10 * 2^20 -- 10 MB

class SerializeConf conf where
    serializeBufSize :: conf -> Int
    serializeBufSize _ = defaultBufSize

fnToByte :: (SerializeConf conf, Typeable a, Typeable b) => conf -> (a -> b) -> Closure ByteString
fnToByte conf fn = toClosure $ encode $ unsafePerformIO $ trySerializeWith fn $ serializeBufSize conf

byteToFn :: (Typeable a, Typeable b) => Closure ByteString -> (a -> b)
byteToFn = unsafePerformIO . deserialize . decode . unClosure