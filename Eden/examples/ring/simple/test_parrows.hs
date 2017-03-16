module Main where

import Parrows.Definition
import Parrows.Future
import Parrows.Skeletons.Topology
import Parrows.Eden

main = print $ ring () (\(x, y) -> (y, x+1)) ([1..3]::[Int])
