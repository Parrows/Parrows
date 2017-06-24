Vanilla iterUntil.

\begin{code}
module VanillaUntil where

import Data.Either
import Data.List (transpose)
import Parallel.Eden (noPe, Trans)
-- import GHC.Conc (numCapabilities)
import Parallel.Skel.EdenSkel (workpoolD)
\end{code}

\begin{code}
iterUntil :: (Trans task, Trans subResult, Trans dataIn, Trans result) => 
             ((task -> subResult) -> [task] -> [subResult]) -> -- ^ map implementation
             dataIn ->                                                   -- ^ input data
             (dataIn -> [task]) ->                   -- ^ splitter function to generate subtasks
             (task -> subResult) -> -- ^ single worker function
             (dataIn -> [subResult] -> Either result [task]) -> -- ^ combine result and
                                                                -- ^ determin whether we are done
             result -- ^ the combined result
iterUntil pmap  d split fworker comb = let tasks = split d
                                           filterRes = theLeft . head . filter (isLeft)
                                           it = iterator pmap fworker comb d
                                       in filterRes $ iterate it $ Right tasks

iterator :: ((task -> subResult) -> [task] -> [subResult]) -> -- ^ map implementation
            (task -> subResult) -> -- ^ single worker function
           (dataIn -> [subResult] -> Either result [task]) -> -- ^ combine result and
                                                              -- ^ determin whether we are done
           dataIn ->               -- ^ initial big task to create more tasks from
           Either result [task] -> -- ^ initial tasks, already created from big task
           Either result [task] -- ^ intermediate (and somewhen: final) result
iterator pmap fworker comb d tasksBox | isLeft tasksBox = tasksBox
                                      | otherwise = let tasks = theRight tasksBox
                                                    in comb d $ pmap fworker $ tasks
\end{code}

Testing with other implementation of parallel instanciating than parmap
\begin{code}
mwUntil :: (Trans task, Trans subResult, Trans dataIn, Trans result) => 
           dataIn ->                                                   -- ^ input data
          (dataIn -> [task]) ->                   -- ^ splitter function to generate subtasks
          (task -> subResult) -> -- ^ single worker function
          (dataIn -> [subResult] -> Either result [task]) -> -- ^ combine result and
                                                             -- ^ determin whether we are done
          result -- ^ the combined result
mwUntil = iterUntil mw
    where mw = workpoolD noPe 1
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
\begin{code}
transpose' (xs:xss) = mzipWith (:) xs (transpose' xss)
transpose' _        = repeat [] 

isLeft = not . isRight                                                     
isRight (Left _)   = False
isRight (Right _)  = True
theLeft (Left a)   = a
theRight (Right a) = a

mzipWith f (x:xs) ~(y:ys) = f x y : mzipWith f xs ys
mzipWith f _ _ = []
\end{code}