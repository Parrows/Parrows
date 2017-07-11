
This is -*- Literate Haskell -*-

\begin{code}
module RepeatUntil where
import Parallel.Eden
import ParMap
import Parallel.Skel.EdenSkel
import Data.List (transpose)
\end{code}

\begin{xcode}
iterUntil :: (Trans local, Trans task, 
              Trans subResult) => 
             localM ->
             [local] ->
             dataIn ->
             (dataIn -> [task]) ->
             (local -> task -> (subResult,local)) ->
             (localM -> [subResult] -> Either result ([task],localM)) ->
             result
iterUntil lm ls d split fworker comb = result   where
  tasks                = split d
--  outss                = parMapF (p fworker) (zip ls taskss)
  outss                = map_farm (p fworker) (zip ls taskss)
  outssComb            = zipWith comb lms (transpose' outss)
  (more,~[end])        = span isRight outssComb
  result               = theLeft end 
  moreTaskssLms        = map theRight more
  (moreTaskss,morelms) = unzip moreTaskssLms
  taskss               = transpose' (tasks:moreTaskss)  
  lms                  = lm : morelms

iterUntilG parmap lm ls d split fworker comb = result   where
  tasks                = split d
  outss                = parmap (p fworker) (zip ls taskss)
  outssComb            = zipWith comb lms (transpose' outss)
  (more,~[end])        = span isRight outssComb
  result               = theLeft end 
  moreTaskssLms        = map theRight more
  (moreTaskss,morelms) = unzip moreTaskssLms
  taskss               = transpose' (tasks:moreTaskss)  
  lms                  = lm : morelms
\end{xcode}

\begin{code}
repeatUntilG :: (Trans local, Trans task, Trans subResult) => 
                ( ( (local, [task]) -> [subResult]) -> [(local, [task])] -> [[subResult]] ) ->
                localM ->
               [local] ->
               dataIn ->
               (dataIn -> [task]) ->
               (local -> task -> (subResult,local)) ->
               (localM -> [subResult] -> Either result ([task],localM)) ->
               result
-- \begin{code}
repeatUntilG parmap lm ls d split fworker comb = result   where
  tasks                = split d
  outss                = parmap (p fworker) (zip ls taskss)
  outssComb            = zipWith comb lms (transpose' outss)
  (more,~[end])        = span isRight outssComb
  result               = theLeft end 
  moreTaskssLms        = map theRight more
  (moreTaskss,morelms) = unzip moreTaskssLms
  taskss               = transpose' (tasks:moreTaskss)  
  lms                  = lm : morelms
\end{code}

\begin{code}
p :: (local -> task -> (subResult,local)) -> (local,[task]) -> [subResult]
p f (local,tasks) = subResults
        where results   = zipWith f locals tasks
              locals    = local : moreLocals
              (subResults, moreLocals) = unzip results

transpose' = transpose'' -- can't use Data.List.transpose!
              
transpose'' (xs:xss) = mzipWith (:) xs (transpose' xss)
transpose'' _        = repeat [] 
                                                     
isRight (Left _)   = False
isRight (Right _)  = True
theLeft (Left a)   = a
theRight (Right a) = a

mzipWith f (x:xs) ~(y:ys) = f x y : mzipWith f xs ys
mzipWith f _ _ = []
\end{code}
