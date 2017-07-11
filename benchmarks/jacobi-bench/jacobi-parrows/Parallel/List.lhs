Das ist eine spezielle Version fuer Data Parallel Haskell

-*- Literate Haskell -*-
\begin{code}
{-# OPTIONS -XTypeSynonymInstances #-}
{-# LANGUAGE PArr, ParallelListComp #-}
module Parallel.List where

import qualified Prelude as P
import Prelude (($), (.))
import qualified Data.List as DL (length, transpose)
import Data.List  ((!!))

import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int
import Data.Array.Parallel.PArray

import GHC.PArr
\end{code}

\begin{codex}
-- darrList :: [:Int:] -> [Int]
darrList dlist = [dlist!:i | i<-[0..lengthP dlist-1] ]

listDArr :: [Int] -> [:Int:]
listDArr list = [: list!!i | i<-enumFromToP 0 (DL.length list-1) :]
\end{codex}
\begin{code}
darrList = fromP
listDArr = toP
\end{code}

das verteilte transpose
\begin{code}
transpose' :: [: [: a :] :] -> [: [: a :] :]
transpose' x | not $ nullP x = let (hs, ts) = splitAtP 1 x
                               in hs +:+ transpose' ts
             | otherwise = [: [: :] :]

transpose'' = mapP listDArr . listDArr . DL.transpose . darrList . mapP darrList
\end{code}