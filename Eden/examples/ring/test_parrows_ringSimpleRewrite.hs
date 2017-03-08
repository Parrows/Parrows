module Main where

import Parrows.Definition
import Parrows.Future
import Parrows.Skeletons
import Parrows.Eden

import Control.Parallel.Eden

-- this is just to verify that neither the parMap implementation, nor the Future implementation is faulty
-- good: it isn't. The Eden Trace has inter process communication and the right amount of messages etc.:
-- 4, processes, 35 Threads, 24 Conversations, 24 Messages

-- | Simple ring skeleton (tutorial version)
-- using remote data for providing direct inter-ring communication
-- without input distribution and output combination
ringSimple      :: (Trans o, Trans i, Trans r) => (i -> r -> (o,r))  -- ^ ring process function
               -> [i] -> [o]      -- ^ input output mapping
ringSimple f is =  os
  where
    (os,ringOuts)  = unzip (parMap () (toFut $ uncurry f) (zip is $ lazy $ rightRotate ringOuts))

main = print $ ringSimple (\x y -> (y, x+1)) ([1..3]::[Int])
