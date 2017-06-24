Testing why the chunking code does not work.

We generate a large matrix and feed it to worker PEs, which map (+1) over it and send the result back.

\begin{code}
{-# OPTIONS -fno-monomorphism-restriction #-}
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

We omit type definition first. Does not help!
Now we omit chunking completely
\begin{code}
-- shuff = 10
toL arr = let ((_, _), (n, m)) = bounds arr
              -- ll = putStrLn "unshuffling matrix..." `pseq`
              --      unshuffleN shuff $ elems arr
              -- lm = rnf ll `pseq` ll
              -- in (n, m, lm)
              ls = elems arr
              -- in ((n, m), ls)
          in n:m:ls
-- fromL ((n, m), xs) = listArray ((1,1), (n, m)) xs
fromL (n:m:xs) = listArray ((1,1), (n, m)) xs
-- fromL ((n, m), xss) = listArray ((1,1), (n, m)) $ 
--                       trace "shuffling matrix..." $
--                       shuffleN xss

liftL f = toL . f . fromL
liftB f = fromL . f . toL
\end{code}

Test matrices.
\begin{code}
m1, m2 :: Int -> MatArr Int Int
m1 n = listArray ((1,1), (n,n)) $ take (n*n) [1..]
m2 n = pascalPerm n
\end{code}

Test function
\begin{code}
dupMat = take (noPe-1) . repeat
-- worker :: (Trans i, Integral i, Ix i, Trans x, Num x, NFData x) =>
--        MatArr i x -> MatArr i x
worker = fmap (+1)
lifted = liftL worker

-- test :: (Trans i, Integral i, Ix i, Trans x, Num x, NFData x) =>
--        MatArr i x -> [MatArr i x]
-- dupMat and toL are currently commutative
-- test = map fromL . (map_farm lifted) . dupMat . toL
test = (map fromL) . (map_farm lifted) . (map toL) . dupMat
\end{code}

Now we omit all this matrix thing.
\begin{code}
worker2 = map (+1)
test2 :: (Trans x, Num x) =>
         Int -> MatArr Int x -> [MatArr Int x]
test2 n = map (listArray ((1,1), (n,n))) . map_farm worker2 . map elems . dupMat
\end{code}

Main.
\begin{code}
main = do
  putStrLn "Begin..."
  args <- getArgs
  let n = read $ head args
      k = read $ args!!1
      m | k==1 || k==3 = m1 n
        | k==2 || k==3 = m2 n
        | otherwise = error "Wrong matrix type!"
      res = case k of
              1 -> test m
              2 -> test m
              3 -> test2 n m
              4 -> test2 n m
  rnf m `seq` putStrLn "Matrix generated."
  rnf res `seq` putStrLn "done"
\end{code}
