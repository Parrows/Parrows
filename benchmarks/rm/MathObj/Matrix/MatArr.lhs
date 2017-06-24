This file is -*- Literate Haskell -*-

Matrizen und Vektoren in Arrays. Dabei: keine Instanz von Trans! Es geht nicht!
\begin{code}
{-# OPTIONS -XTypeSynonymInstances #-}
module MathObj.Matrix.MatArr where

import qualified Data.Array as DA
import Data.Array ((!), (//), bounds, Ix, listArray, array, Array, range)
-- import Data.Array.Diff ((!), (//), bounds, Ix, listArray, array, Array, range)
import Debug.Trace (trace)
import Data.List
\end{code}

Datenformat. Die Daten werden _linear_ in Speicher gehalten, die Indizes rechnen wir um.
\begin{code}
type MatArr a b = DA.Array (a, a) b
\end{code}

As in Gentle Introduction!
http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/arrays.html

\begin{code}
-- matMult         :: (Ix a, Ix b, Ix c, Integral a, Integral b, Integral c, Num d) =>
--                   Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult :: (Ix i, Integral i, Num x) =>
           MatArr i x -> MatArr i x -> MatArr i x
matMult x y     =  let (n, m) = checkRanges x y
                       res = [ ((i,j), 
                                     zeileMalSpalte [ (x!(i,k), y!(k,j)) | k<-[1..m] ] )
                               | i<-[1..n], j<-[1..n] ]
                   in array ((1, 1), (n, m)) res
checkRanges x y = let ((ln1, lm1), (n1, m1)) = bounds x
                      ((ln2, lm2), (n2, m2)) = bounds y
                  in if (ln1==ln2 && lm1==lm2 &&
                         n1==n2 && m1==m2)
                     then (n1, m2)
                     else error "check matrix bounds!"
zeileMalSpalte :: Num x => [(x, x)] -> x
zeileMalSpalte = sum . uncurry (zipWith (*)) . unzip
\end{code}

Zugriff
\begin{code}
getRow i xss = let ((_, lm), (n, m)) = bounds xss
               in [ xss!(i,j) | j<-[lm..m] ]
getCol j xss = let ((ln, lm), (n, _)) = bounds xss
               in [ xss!(i,j) | i<-[ln..n] ]
-- instance (Ix a, Show a, Show b) => Show (MatArr a b) where
showMat xss = let ((_, lm), (n, m)) = bounds xss
                  showRows = unlines $ map (\i -> (show $ getRow i xss) ++ ",") [lm..m]
              in "[" ++ (showRows) ++ "]"
\end{code}
