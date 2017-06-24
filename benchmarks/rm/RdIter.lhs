This is -*- Literate Haskell -*-

A new iteration skeleton with remote data.
\begin{code}
module RdIter (iterUntil2) where

import Parallel.Eden
import RemoteData
import Data.Maybe
\end{code}

get task. split task in subtasks. process subtasks. look of done. if not: create more subtasks. process them...

type CreateTasks task subTask = task -> Maybe [subTask] -> [subTask]
type Worker subTask subResult = subTask -> subResult
type Combine subResult task result = [subResult] -> Maybe task -> Maybe result
iterUntilRD :: (CreateTasks task subTask subResult) 
            -> (Worker subTask subResult) 
            -> (Combine subResult task result) 
            -> task -> result
\begin{code}
iterUntil2 createTasks worker combine x = fetch $ iterUntilRD createTasks' worker' combine' x'
    where x' = release x
          worker' = liftRD worker
          createTasks' t Nothing = releaseAll $ createTasks (fetch t) Nothing
          createTasks' t (Just ms) = releaseAll $ createTasks (fetch t) (Just (fetchAll ms))
          combine' srs Nothing = release $ combine (fetchAll srs) Nothing
          combine' srs (Just t) = release $ combine (fetchAll srs) (Just (fetch t))
\end{code}
type LCreateTasks task subTask subResult = RD task -> Maybe [RD subTask] -> [RD subTask]
type LWorker subTask subResult = RD subTask -> RD subResult
type LCombine subResult task result  = [RD subResult] -> Maybe (RD task) -> Maybe (RD result)
iterUntilRD :: (LCreateTasks task subTask subResult) 
            -> (LWorker subTask subResult) 
            -> (LCombine subResult task result) 
            -> RD task -> RD result
\begin{code}
iterUntilRD :: (RD task -> Maybe [RD subTask] -> [RD subTask]) ->
               (RD subTask -> RD subResult) ->
              ([RD subResult] -> Maybe (RD task) -> (RD (Maybe result))) ->
              RD task -> RD result
iterUntilRD createTasks worker combine x = let initBatch = getRightNumTasks createTasks x
                                               
                                           in undefined

getRightNumTasks createTasks x = let nope = max noPe 1
                                     initSubTasks = createTasks x Nothing
                                     subtasks = initSubTasks ++ moreSubTasks
                                     (initBatch, moreSubTasks) = splitAt nope subtasks
                                 in initBatch
\end{code}