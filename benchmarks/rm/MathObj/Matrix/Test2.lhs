Test Gauss with residue mappings of vulgar fractions.

This is -*- Literate Haskell -*_
\begin{code}
module Main where

import Data.Array
import MathObj.Matrix.MatArr
import MathObj.Generic.VulgarFraction
import System.Random
import MathObj.Matrix.Gauss
import MathObj.Matrix.Residue
import MathObj.Primes.Determ
import MathObj.Residue.FracMulti (nFromM)

import Control.Parallel
import Control.Parallel.Strategies
import System (getArgs)
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

-- instance (NFData a) => NFData (Fraction a)

test n l k = do 
  print "Beginning..."
  g <- newStdGen
  let m = genMatrixFrac n g
      r = gauss m
  -- putStrLn $ showMat m
  rnf m `seq` print "Matrix generated"
  -- putStrLn $ showMat r
  rnf r `seq` print "Conventional Gauss computed"
  let d = product $ diag $ r
  print d
  putStrLn $ "Requied bound: " ++ (show $ maxFarey d)
  let -- r2 = gaussResidue ps m
      ps = take k $ primesFrom l
      -- ps2 = primesScaleMax (ceiling $ evalf d) l
  -- print "Generating primes" `seq` ps `using` rnf `seq` print "Done primes"
  print ps
  putStrLn $ "Bounds: " ++ (show $ nFromM $ product ps)
  print $ detResidue ps m
  print "done"
\end{code}

Main.

\begin{code}
main = do
  args <- getArgs
  let n = if (length args<3) then error "Need 3 parameters"
          else read $ head args
      l = read $ args!!1
      k = read $ args!!2
  test n l k
\end{code}