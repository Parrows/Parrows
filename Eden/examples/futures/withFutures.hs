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
module Main where

import Parrows.Definition
import Parrows.Eden
import Parrows.Future

import Control.DeepSeq

import Control.Arrow

main = print $ length $ (rnf val) `seq` val
           where
               val = (parEvalN () (map (>>> put) $ replicate 4 $ (+1))) >>> rightRotate >>> (parEvalN () (map (get >>>) $ replicate 4 $ (*3))) $ ([1..16]::[Int])

-- from Eden, ported to Arrows:
rightRotate :: (Arrow arr) => arr [a] [a]
rightRotate = arr $ \list -> case list of [] -> []
                                          xs -> last xs : init xs