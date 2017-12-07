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
module Parrows.Util where

import           Control.Arrow

import           Data.List

--import Control.DeepSeq

zipWithArr :: ArrowChoice arr => arr (a, b) c -> arr ([a], [b]) [c]
zipWithArr zipFn = arr (uncurry zip) >>> mapArr zipFn

listApp :: (ArrowChoice arr) => [arr a b] -> arr [a] [b]
listApp (f:fs) = arr listcase >>>
         arr (const []) ||| (f *** listApp fs >>> arr (uncurry (:)))
         where listcase []     = Left ()
               listcase (x:xs) = Right (x,xs)
listApp [] = arr (const [])

-- from http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr = listApp . repeat

-- fold on Arrows inspired by mapArr
foldlArr :: (ArrowChoice arr, ArrowApply arr) => arr (b, a) b -> b -> arr [a] b
foldlArr f b = arr listcase >>>
             arr (const b) ||| (first (arr (\a -> (b, a)) >>> f >>> arr (foldlArr f)) >>> app)
             where listcase []     = Left []
                   listcase (x:xs) = Right (x,xs)

-- From Eden:

-- okay. (from: https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/src/Control-Parallel-Eden-Auxiliary.html#unshuffle)
unshuffle :: (Arrow arr) => Int -> arr [a] [[a]]
unshuffle n = arr (\xs -> [takeEach n (drop i xs) | i <- [0..n-1]])

takeEach :: Int -> [a] -> [a]
takeEach n []     = []
takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)

-- | Simple shuffling - inverse to round robin distribution
shuffle :: (Arrow arr) => arr [[a]] [a]
shuffle = arr (concat . transpose)

-- | A lazy list is an infinite stream
lazy :: (Arrow arr) => arr [a] [a]
lazy = arr (\ ~(x:xs) -> x : lazy xs)

{-data Lazy a = Lazy a

mkLazy :: (Arrow arr) => arr a (Lazy a)
mkLazy = arr (\a -> Lazy a)

unLazy :: (Arrow arr) => arr (Lazy a) a
unLazy = arr(\(Lazy a) -> a)-}
