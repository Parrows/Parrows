{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Main where

import Parrows.Definition
import Parrows.Future
import Parrows.Topology
import Parrows.Eden

import Control.Arrow

import Control.Parallel.Eden

-- this is just to verify that neither the parMap implementation, nor the Future implementation is faulty
-- good: it isn't. The Eden Trace has inter process communication and the right amount of messages etc.:
-- 4, processes, 35 Threads, 24 Conversations, 24 Messages

-- | Simple ring skeleton (tutorial version)
-- using remote data for providing direct inter-ring communication
-- without input distribution and output combination
ringSimple' :: (Trans r, ArrowLoop arr, ArrowApply arr, Future RemoteData r, (ArrowParallel arr (i, RemoteData r) (o, RemoteData r) conf)) =>
               conf
               -> arr (i, r) (o,r) -- ^ ring process function
               -> arr [i] [o]      -- ^ input output mapping
ringSimple' conf f = loop $ second (arr rightRotate >>> arr lazy) >>> (arr $ uncurry zip) >>> (parMap conf (toFut $ f)) >>> arr unzip


main = print $ ringSimple' () (\(x, y) -> (y, x+1)) ([1..3]::[Int])
