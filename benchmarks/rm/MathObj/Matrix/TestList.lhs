Testing why the chunking code does not work.

We generate a large matrix and feed it to worker PEs, which map (+1) over it and send the result back.

\begin{code}
module Main where

-- import MathObj.Matrix.Residue
import Parallel.Eden
-- import Parallel.SkelHelper
import Parallel.Skel.EdenSkel
import MathObj.Matrix.MatArr
import Data.Array
import MathObj.Matrix.Pascal
import System (getArgs)
import Control.Parallel
import Debug.Trace
\end{code}

Transmission code, manually pasted from MO.M.Residue.

\begin{oldcode}
toL :: (Ix i, Num n, NFData n) => MatArr i n -> TransMat i n
toL arr = let ((_, _), (n, m)) = bounds arr
              ll = putStrLn "unshuffling matrix..." `pseq`
                   unshuffleN shuff $ elems arr
              lm = rnf ll `pseq` ll
              -- in (n, m, lm)
          in ((n, m), lm)
fromL :: (Ix i, Num i, Num n) => TransMat i n -> MatArr i n
-- fromL (n, m, xs) = listArray ((1,1), (n, m)) $ 
fromL ((n, m), xss) = listArray ((1,1), (n, m)) $ 
                      trace "shuffling matrix..." $
                      shuffleN xss

liftL :: (Ix i, Num i, Num n, NFData n) => (MatArr i n -> MatArr i n ) -> TransMat i n -> TransMat i n
liftL f = toL . f . fromL
liftB f = fromL . f . toL
\end{oldcore}


Test function
\begin{code}
dupMat :: Trans x => x -> [x]
dupMat a = let x = take noPe $ repeat a
           in rnf x `seq` x
\end{code}

Rest omitted, see below for a pure list implementation
\begin{nocode}
-- worker :: (Trans i, Integral i, Ix i, Trans x, Num x, NFData x) =>
--        MatArr i x -> MatArr i x
worker = fmap (+1)
lifted = liftL worker

-- test :: (Trans i, Integral i, Ix i, Trans x, Num x, NFData x) =>
--        MatArr i x -> [MatArr i x]
-- dupMat and toL are currently commutative
-- test = map fromL . (map_farm lifted) . dupMat . toL
test = (map fromL) . (map_farm lifted) . (map toL) . dupMat
\end{nocode}

Now we omit all this matrix thing.
\begin{code}
m1 :: Int -> [Int]
m1 n = take (n*n) [1..]
worker2 = map (+1)
test2 :: [Int] -> [[Int]]
test2 = map_par worker2 . dupMat
-- test2 = farm noPe (process worker2) . dupMat
\end{code}

Main.
\begin{code}
main = do
  putStrLn "Begin..."
  args <- getArgs
  let n = read $ head args
      k = read $ args!!1
      m | k==3 = m1 n
        | otherwise = error "Wrong matrix type!"
      res = case k of
              3 -> test2 m
  rnf m `seq` putStrLn "Matrix generated."
  rnf res `seq` putStrLn "done"
\end{code}
