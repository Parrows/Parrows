module Main where

import Control.Parallel.Eden
import Control.Parallel.Eden.Topology

main = print $ ringSimple (\x y -> (y, x+1)) ([1..3]::[Int])
