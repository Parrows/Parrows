module Main where

import Control.Parallel.Eden
import Control.Parallel.Eden.Map

main :: IO ()
main = do putStrLn $ show $ farmS 4 (+1) ([1..]::[Int])
