{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import Control.Arrow
import Parrows.Definition
import Parrows.ParMonad

add :: (ArrowParallel arr a Int (),
        ArrowApply arr,
        ArrowChoice arr)
    => arr a Int -> arr a Int -> arr a Int
add f g = f |&&&| g >>> (arr $ \(u, v) -> u + v)

main = print $ (add (+1) (+2)) (2::Int)