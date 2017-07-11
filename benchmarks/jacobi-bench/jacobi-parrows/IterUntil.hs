module IterUntil(iterUntil) where
import Parallel.Eden
import ParMap
import Parallel.Skel.EdenSkel

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

              
transpose' (xs:xss) = mzipWith (:) xs (transpose' xss)
transpose' _        = repeat [] 
                                                     
isRight (Left _)   = False
isRight (Right _)  = True
theLeft (Left a)   = a
theRight (Right a) = a

mzipWith f (x:xs) ~(y:ys) = f x y : mzipWith f xs ys
mzipWith f _ _ = []