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
module Parrows.HdpH where

import Parrows.Definition
import Control.Arrow

import Data.Maybe

import Control.Applicative
import Control.Monad

import Control.DeepSeq

import Control.Parallel.HdpH
import Control.Parallel.HdpH.Strategies

import Data.Binary(encode, decode)
import Data.ByteString.Lazy(ByteString)
import Data.Typeable

import GHC.Packing.Core
import GHC.Packing.Type

import System.IO.Unsafe (unsafePerformIO)

-- TODO: check whether it is okay that we spawn "exponentially"

-- class to extract the RTSConf out of conf
class HdpHConf conf where
    rtsConf :: conf -> RTSConf

-- obvious instance for RTSConf
instance HdpHConf RTSConf where
    rtsConf = id

type FunctionTypeToken a b = [a -> b]

-- class to extract the Strategy out of the conf
class HdpHStrategy conf b where
    strategy :: conf -> Serialized (Strategy (Serialized b))


-- default implementation for basic RTSConf, uses forceCC as the strategy
instance (NFData b) => HdpHStrategy RTSConf b where
    strategy _ = forceCC


-- | default buffer size used by trySerialize
defaultBufSize :: Int
defaultBufSize = 10 * 2^20 -- 10 MB

class SerializeConf conf where
    serializeBufSize :: conf -> Int
    serializeBufSize _ = defaultBufSize

instance SerializeConf RTSConf


instance (SerializeConf conf, HdpHConf conf, HdpHStrategy conf b, Typeable b, NFData b) => ArrowParallel (->) a b conf where
     parEvalN conf fs as = fromJust' $ unsafePerformIO $ runParIO (rtsConf conf) $
                 -- apply the functions to the inputs, serialize the not yet computed state so that we can send this over the network
                 -- and put it into a [Closure (ByteString)], this is then fully evaluated with parClosureListSerialized
                 -- but stays a [Closure (ByteString)]
                 do clo_bs_evaluated <- map (unsafePerformIO . (flip trySerializeWith $ serializeBufSize conf)) (zipWith ($) fs as)
                                        `using`
                                        parClosureList (strategy conf)
                    -- decode [Closure (ByteString)] to [b]
                    return $ map (unsafePerformIO . deserialize) clo_bs_evaluated
                             where fromJust' :: Maybe [b] -> [b]
                                   -- just to make sure that this doesn't throw an error
                                   fromJust' Nothing = []
                                   fromJust' bs = fromJust bs