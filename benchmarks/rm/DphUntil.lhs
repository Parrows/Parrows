iterUntil. A new version. A special DPH version.

\begin{code}
module DphUntil where

import GHC.Conc (numCapabilities)
-- import Data.Either
import Data.List (transpose)

\end{code}

\begin{code}
import Parallel.List
import qualified Prelude as P
import Prelude ((.), ($), Num, print, putStrLn, Integer, error, iterate)
import Data.Array.Parallel.Prelude 
import qualified Data.Array.Parallel.Prelude.Int as I
import Data.Array.Parallel.Prelude.Int
import Data.Array.Parallel.PArray
import Data.Maybe
{- DEPRICATED -}
map_dph :: (a -> b) -> [a] -> [b]
map_dph f = darrList . mapP f . listDArr

headP xs = xs!:0

--iterateP :: Int -> (a -> a) -> a -> [: a :]
iterateP k f x | k>0 = let r = f x
                       in (singletonP r) +:+ iterateP (k-1) f r
               | otherwise = singletonP $ f x

type Either a b = (Bool, a, b)
theLeft (False, a, _) = a
theLeft (True, _, _) = error "left: getting right"
theRight (True, _, b) = b
theRight (False, _, _) = error "right: getting left"
isRight (b, _, _) = b
isLeft = not . isRight
-- makeRight x = (True, _, x)
-- makeLeft x = (False, x, _)
\end{code}

\begin{code}
{-
iterUntil :: ((task -> subResult) -> [: task :] -> [: subResult :]) -> -- ^ map implementation
             dataIn ->                                                   -- ^ input data
             (dataIn -> [: task :]) ->                   -- ^ splitter function to generate subtasks
             (task -> subResult) -> -- ^ single worker function
             (dataIn -> [: subResult :] -> Either result [: task :]) -> -- ^ combine result and
                                                                -- ^ determin whether we are done
             result -- ^ the combined result
-}
iterUntil k pmap  d split fworker comb = let tasks = split d
                                             filterRes = theLeft . headP . filterP (isLeft)
                                             it = iterator pmap fworker comb d
                                         in filterRes $ iterateP k it $ (True, Nothing, tasks)
{-
iterator :: ((task -> subResult) -> [: task: ] -> [: subResult :]) -> -- ^ map implementation
            (task -> subResult) -> -- ^ single worker function
           (dataIn -> [: subResult :] -> Either result [: task :]) -> -- ^ combine result and
                                                              -- ^ determin whether we are done
           dataIn ->               -- ^ initial big task to create more tasks from
           Either result [: task :] -> -- ^ initial tasks, already created from big task
           Either result [: task :] -- ^ intermediate (and somewhen: final) result
-}
iterator pmap fworker comb d tasksBox | isLeft tasksBox = tasksBox
                                      | otherwise = let tasks = theRight tasksBox
                                                    in comb d $ pmap fworker $ tasks
\end{code}
  tasks                = split d
--  outss                = parMapF (p fworker) (zip ls taskss)
  outss                = map_par (p fworker) (zip ls taskss)
  outssComb            = zipWith comb lms (transpose' outss)
  (more,~[end])        = span isRight outssComb
  result               = theLeft end 
  moreTaskssLms        = map theRight more
  (moreTaskss,morelms) = unzip moreTaskssLms
  taskss               = transpose' (tasks:moreTaskss)  
  lms                  = lm : morelms

  
p :: (local -> task -> (subResult,local)) -> (local,[task]) -> [subResult]
p f (local,tasks) = subResults
        where results   = zipWith f locals tasks
              locals    = local : moreLocals
              (subResults, moreLocals) = unzip results


Helpers:
transpose''' (xs:xss) = mzipWith (:) xs (transpose''' xss)
transpose''' _        = repeat [] 
\begin{code}

-- isLeft = not . isRight                                                     
-- isRight (Left _)   = False
-- isRight (Right _)  = True
-- theLeft (Left a)   = a
-- theRight (Right a) = a
\end{code}
mzipWith f (x:xs) ~(y:ys) = f x y : mzipWith f xs ys
mzipWith f _ _ = []
