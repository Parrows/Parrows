Test Gauss with residue mappings of vulgar fractions. Parallel version.

This is -*- Literate Haskell -*_
\begin{code}
{-# OPTIONS -cpp -XTypeSynonymInstances #-}
#ifdef INTERACT
module MathObj.Matrix.Test3 where
#else
module Main where
#endif

import Data.Array
import MathObj.Matrix.MatArr
import MathObj.Generic.VulgarFraction
import System.Random
import MathObj.Matrix.Gauss
import MathObj.Matrix.Residue
import MathObj.Primes.Determ
import MathObj.Residue.FracMulti (nFromM)
import MathObj.Matrix.Pascal

import Control.Parallel
import System (getArgs)

import Debug.Trace (trace)

#define __EDEN__ 1
#ifdef __EDEN__
import Control.Parallel.Strategies hiding (rnf)
import Parallel.Eden
import Parallel.Skel.EdenSkel
import MathObj.Residue.FracMulti (FSingleMod)
#else
import Control.Parallel.Strategies
#endif
\end{code}

\begin{code}
type Frac = Fraction Int
-- instance (NFData a) => NFData (Fraction a)
-- instance (NFData a, NFData i, Ix i) => NFData (Array i a) -- defined in C.P.S.
#ifdef __EDEN__
instance (Trans a, Integral a) => Trans (Fraction a)
-- instance (Trans a, Trans i, Ix i) => Trans (MatArr i a)
instance (Trans a, NFData a) => Trans (FSingleMod a)
instance (NFData a) => NFData (FSingleMod a)
#endif
\end{code}
\begin{code}
genMatrixFracBig :: (RandomGen r) => Int -> r -> MatArr Int (Fraction Integer)
genMatrixFracBig n g = let rs = take (2*n^2) $ filter (/= 0) $ randomRs (-50,50) g
                           (noms, denoms) = splitAt (n^2) rs
                           fracs = zipWith (/:/) noms denoms
                           arr = listArray ((1,1),(n,n)) fracs
                       in arr

-- genMatrixFrac :: (Integral i) => Int -> MatArr Int (Fraction i)
genMatrixFrac' n = let rs = map fromIntegral [1..]
                       (noms, denoms) = (rs, tail rs)
                       fracs = zipWith (/:/) noms denoms
                       arr = listArray ((1,1),(n,n)) fracs
                   in arr

genMatrixFrac n = fmap (flip (/:/) 3) $ pascalPerm n
\end{code}
-- from LinSolv
-- #warning "Not really fractions!"
genMatrixFrac'' n = listArray ((1,1),(n,n)) $ take (n*n) $ concat [
	     [63403,9598,48460,38558,55945,63604,26902,43606,6700,0,29932,30620,55211,22253],
	     [11459,50088,21826,28974,37628,29928,10075,459,11727,0,47588,2146,44458,17709],
	     [1957,54440,615,0,30112,30089,59182,6749,16574,46584,0,37503,46368,58767],
	     [16556,16339,16684,11161,4842,0,23337,18183,0,39431,53482,2907,3430,17385],
	     [977,63851,16994,37539,3637,41366,38849,62644,36993,2068,44228,55767,54558,25955],
	     [21854,8124,48200,18022,36006,535,33901,28364,39630,8019,11499,12646,50905,27263],
	     [0,8990,50466,11609,10151,50269,9350,51470,0,23905,56340,1605,49558,9010],
	     [0,52719,23811,10935,11322,0,14616,22137,12272,65244,0,49744,26807,29990],
	     [19561,21251,8417,1415,10603,53021,16919,30306,52192,61265,54920,0,6229,14462],
	     [15428,26241,34092,9594,45355,0,58218,35352,55015,3303,0,58401,12641,3272],
	     [0,44099,14208,63540,42473,32166,0,52439,34352,36121,19287,9063,15106,12081],
	     [1797,35580,35733,64668,54318,50085,6352,29227,14499,30944,49080,15862,31642,9219],
	     [56294,33740,36848,8327,17964,52692,25761,0,28293,47062,55864,39471,0,19574],
	     [0,30671,53596,0,0,18839,28239,34609,13841,18952,0,0,5342,127] ]
         {- determinant a: -2336674347265427867508388115964260778566383985290241186947292168676 % 1 -}
\begin{code}
genMatrixInt n g = let rs = map (fromIntegral) $ take (n^2) $ randomRs (0,2*n) g
                       fracs = zipWith (/:/) rs [1,1..]
                       arr = listArray ((1,1),(n,n)) fracs
                   in arr

type Map = (TransMat Int (FSingleMod Integer) -> TransMat Int (FSingleMod Integer)) -> [TransMat Int (FSingleMod Integer)] -> [TransMat Int (FSingleMod Integer)]
\end{code}

\begin{code}
main = do
  args <- getArgs
  putStrLn $ unlines $ ["beatSolve, test3. A parallel Gauss elimination in Eden",
                        "Need 4 parameters.",
                        "<type> <length> <starting prime> <prime count>",
                        "type:",
                        " 1 - rational BROKEN",
                        " 2 - sequential redisue",
                        " 3 - parMap residue",
                        " 4 - workpoolD residue BROKEN",
                        " 5 - farm residue",
                        " 6 - DM residue",
                        " 7 - workpool residue BROKEN",
                        "length: integer, length x length matrix is computed",
                        "we take <prime count> primes greather or equal <starting prime>"]
  let t,n,k :: Int
      l :: Integer
      t = read $ args!!0
      n = read $ args!!1
      l = read $ args!!2
      k = read $ args!!3
#ifdef __EDEN__
      nope = max (noPe - 1) 1
#else
      nope = 1
#endif
  putStrLn $ "no. workers = " ++ show nope
\end{code}
map_par, map_farm, map_dm, map_wp :: (Trans a , Trans b) => 
				       (a -> b) -> [a] -> [b]
\begin{xcode}
  case t of
    1 -> testFrac n
    2 -> testG (map) n l k
#if 0
    3 -> testG (map_par) n l k
    -- 4 -> testG (workpoolD nope 3) n l k
    5 -> testG (map_farm) n l k
    6 -> testG (map_dm) n l k
    7 -> testG (map_wp) n l k
#endif
    _ -> error "Wrong type selected!"
\end{xcode}
\begin{code}
  putStrLn "Beginning..."
  g <- newStdGen
  let m :: MatArr Int (Fraction Integer)
      m = genMatrixInt n g
      -- m = genMatrixFrac n
  rnf m `seq` putStrLn "Matrix generated"
  putStrLn $ showMat m
  let ps = take k $ primesFrom l
  putStrLn "Generating primes..."
  putStrLn $ "Primes: " ++ show ps
  putStrLn $ "Bound: " ++ (show $ nFromM $ product ps)
  putStrLn $ "PEs: " ++ show noPe
  putStrLn "Computing..."
  let res :: Fraction Integer      
{-
      res = detResidueG mf ps m
      mf :: (TransMat Int (FSingleMod Integer) -> TransMat Int (FSingleMod Integer)) -> [TransMat Int (FSingleMod Integer)] -> [TransMat Int (FSingleMod Integer)]
      -- mf = map_farm
      mf = case t of
             2 -> map
             3 -> map_par
             5 -> map_farm'
             6 -> map_dm
             7 -> map_wp
             _ -> error "Wrong type!"
-}
      res = case t of
              2 -> detResidueG (map'' :: Map) ps m
              3 -> detResidueG (map_par'' :: Map) ps m
              4 -> detResidueG (workpoolD'' nope 3 :: Map) ps m
              5 -> detResidueG (map_farm'' :: Map) ps m
              6 -> detResidueG (map_dm'' :: Map) ps m
              7 -> detResidueG (map_wp''' :: Map) ps m
              _ -> error "wrong type"
  print res
  putStrLn "done"

-- map_farm' = trace "farm" $ map_farm
showName s f = trace s $ f
map'' = showName "seq. map" map
map_par'' = showName "parmap" map_par'
workpoolD'' = showName "workpool 1" $ workpoolD
map_farm'' = showName "farm" $ map_farm'
map_dm'' = showName "dm" $ map_dm'
map_wp''' = showName "workpool 2" $ map_wp''
\end{code}
