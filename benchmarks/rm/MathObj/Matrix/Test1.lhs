Test Gauss with vulgar fractions.

This is -*- Literate Haskell -*_
\begin{code}
module Test where

import Data.Array
import MathObj.Matrix.MatArr
import MathObj.Generic.VulgarFraction
import System.Random

import MathObj.Residue.FracMulti
\end{code}

\begin{code}
type Frac = Fraction Int
genMatrixFracBig :: (RandomGen r) => Int -> r -> MatArr Int Frac
genMatrixFracBig n g = let rs = take (2*n^2) $ randomRs (-100,100) g
                           (noms, denoms) = splitAt (n^2) rs
                           fracs = zipWith (/:/) noms denoms
                           arr = listArray ((1,1),(n,n)) fracs
                       in arr

genMatrixFrac n _ = let rs = [1..]
                        (noms, denoms) = (rs, tail rs)
                        fracs = zipWith (/:/) noms denoms
                        arr = listArray ((1,1),(n,n)) fracs
                    in arr


genMatrixInt n g = let rs = take (n^2) $ randomRs (0,2*n) g
                       fracs = zipWith (/:/) rs [1,1..]
                       arr = listArray ((1,1),(n,n)) fracs
                   in arr

-- genResidues ps matrix = fmap (makeFZ)

test n = do 
  g <- newStdGen
  let m = genMatrixFrac n g
      r = gauss m
  putStrLn $ showMat m
  putStrLn $ showMat r
  print $ product $ diag $ r
  print "done"
\end{code}