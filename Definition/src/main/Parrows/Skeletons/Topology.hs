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
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Parrows.Skeletons.Topology where

import Control.Arrow

import Parrows.Definition
import Parrows.Future
import Parrows.Util

import Parrows.Skeletons.Map

-- Ports of Control.Parallel.Eden.Topology to Parrows:

pipe :: (ArrowLoop arr, ArrowApply arr, ArrowParallel arr (fut a) (fut a) conf, Future fut a) => conf -> [arr a a] -> arr a a
pipe conf fs = unliftFut $ pipeFut conf fs

-- FIXME: does this really need arrowloop?
-- FIXME: this could probably be expressed
-- FIXME: similarly to mapArr or foldlArr in the Util module
pipeFut :: (ArrowLoop arr, ArrowApply arr, ArrowParallel arr (fut a) (fut a) conf, Future fut a) => conf -> [arr a a] -> arr (fut a) (fut a)
pipeFut conf fs = resolve (arr $ \(a, outs) -> lazy $ a : outs) (parEvalNFut conf fs) >>> arr last
    where
        -- util for the infinite resolve fix place recursion with ArrowLoop(s)
        resolve :: (ArrowApply arr, ArrowLoop arr) => arr (a, b) c -> arr c b -> arr a b
        resolve transform f = loop $ (arr $ \(a, b) -> (b, (f, (transform, (a, b))))) >>> second (second app >>> app)

ring :: (ArrowLoop arr, ArrowApply arr, Future fut r, (ArrowParallel arr (i, fut r) (o, fut r) conf)) =>
    conf ->
    arr (i, r) (o, r) ->
    arr [i] [o]
ring conf f = loop $ second (arr rightRotate >>> arr lazy) >>> (arr $ uncurry zip) >>> (parMap conf (toFut $ f)) >>> arr unzip

-- similar to Eden's toRD
toFut :: (Arrow arr, Future fut r) =>
        (arr (i, r) (o, r))              -- ^ ring process function
        -> (arr (i, fut r) (o, fut r))   -- ^ with remote data
toFut f = (second $ arr get) >>> f >>> (second $ arr put)


torus :: (ArrowLoop arr,
            ArrowParallel arr (c, fut [a], fut [b]) (d, fut [a], fut [b]) conf,
            ArrowParallel arr [(c, fut [a], fut [b])] [(d, fut [a], fut [b])] conf,
            Future fut [a], Future fut [b]) =>
         conf ->
         arr (c, [a], [b]) (d, [a], [b]) -- ^ node function
         -> arr [[c]] [[d]]                -- ^ input-output mapping
torus conf f = loop $ second (arr (map rightRotate) *** arr rightRotate) >>>
                        arr (\ ~(inss, (inssA, inssB)) -> zipWith3 zip3 inss (lazy inssA) (lazy inssB)) >>>
                        parEvalNM conf (repeat (repeat (ptorus f))) >>>
                        arr (map unzip3) >>> arr unzip3 >>> threetotwo

ptorus :: (Arrow arr, Future fut [a], Future fut [b]) =>
          arr (c, [a], [b]) (d, [a], [b]) ->
          arr (c, fut [a], fut [b]) (d, fut [a], fut [b])
ptorus f = arr (\ ~(c, fas, fbs) -> (c, get fas, get fbs)) >>> f >>> arr (\ ~(c, as, bs) -> (c, put as, put bs))


threetotwo :: (Arrow arr) => arr (a, b, c) (a, (b, c))
threetotwo = arr $ \ ~(a, b, c) -> (a, (b, c))

twotothree :: (Arrow arr) => arr (a, (b, c)) (a, b, c)
twotothree = arr $ \ ~(a, (b, c)) -> (a, b, c)


-- from Eden, ported to Arrows:
rightRotate :: (Arrow arr) => arr [a] [a]
rightRotate = arr $ \list -> case list of [] -> []
                                          xs -> last xs : init xs