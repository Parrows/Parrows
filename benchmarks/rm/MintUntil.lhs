iterUntil (Fernando) -> VanillaUntil -> MintUntil.

This is -*- Literate Haskell -*-

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}
module MintUntil where

import GHC.Conc (numCapabilities)
import Parallel.Eden (selfPe, noPe, Trans, NFData)
import Parallel.Skel.EdenMWToken (mw, mwN, mwRingSLF, mwRingSLF')
import Data.Either
import Data.List (transpose)
import Data.Maybe

-- import Debug.Trace (trace)
trace _ x = x
\end{code}

the basics
\begin{code}
data State = Working | Abort
             deriving (Eq, Show)
instance NFData State
instance Trans State
instance Ord State where
    (<=) Abort Working = False
    (<=) _ _ = True

lazyMapMaybe :: (Trans a, Trans b) =>
                (a -> Maybe b) -> [a] -> [b]
lazyMapMaybe f (x:xs) = let y = f x
                            ys = lazyMapMaybe f xs
                        in case y of
                             Nothing -> []
                             Just r -> r:ys
lazyMapMaybe _ [] = []

lazyMapMaybeState' :: (Trans a, Trans b) =>
                     (a -> Maybe b) -> [a] -> State -> [(Maybe b, State)]
lazyMapMaybeState' f (x:xs) Working = let y = f x
                                          ys = lazyMapMaybeState' f xs Working
                                      in case y of
                                           Nothing -> [(Nothing, Abort)]
                                           Just r -> (y, Working):ys
lazyMapMaybeState' _ _ Working = [(Nothing, Working)]
lazyMapMaybeState' _ _ Abort = [(Nothing, Abort)]

concatState :: [(Maybe b, State)] -> ([b], State)
concatState xs = let (bs, ss) = unzip xs
                 in concatHelper bs [] ss
concatHelper ((Just x):xs) ys (Working:ss) = concatHelper xs (x:ys) ss
concatHelper ((Nothing):xs) ys (Working:ss) = concatHelper xs ys ss
concatHelper _ ys (Working:_) = (reverse ys, Working)
concatHelper _ ys (Abort:_) = (ys, Abort)
concatHelper _ _ _ = trace "concatHelper throws!" $ error "concatHelper"

lazyMapMaybeState :: (Trans a, Trans b) =>
                     (a -> Maybe b) -> [a] -> State -> ([b], State)
lazyMapMaybeState f xs s = concatState $ lazyMapMaybeState' f xs s

lazyState :: (Trans a, Trans b) =>
             (a -> b) -> a -> State -> (Maybe b, State)
lazyState f x Working = (Just (f x), Working)
lazyState _ _ Abort = (Nothing, Abort)

-- ^ Function aborts if $f$ returns $Nothing$
lazySelfState :: (Trans a, Trans b) =>
                 (a -> Maybe b) -> a -> State -> (Maybe b, State) 
lazySelfState f x Working = let y = f x
                            in case y of
                                 Nothing -> (Nothing, Abort) -- sic!
                                 Just r -> (y, Working)
lazySelfState _ _ Abort = (Nothing, Abort)


-- ^ Function aborts if $f$ returns $Nothing$
lazySelfStateM :: (Trans a, Trans b) =>
                  (a -> Maybe b) -> a -> State -> Maybe (b, State) 
lazySelfStateM f x Working = let y = f x
                            in case y of
                                 Nothing -> Nothing
                                 Just r -> Just (r, Working)
lazySelfStateM _ _ Abort = Nothing
-- lazySelfStateM _ _ _ = trace "lazySelfStateM: got something strange in input" $
--                       Nothing -- ok

-- lazySelfMap :: (Trans a, Trans b) =>
--                (a -> Maybe b) -> [a] -> State -> ([b], State)
-- lazySelfMap f (x:xs) s = lazySelfState f x s
\end{code}


Stateful taskforce: transform (a -> Bool) to a stateful list mapper
\begin{bad}
eagerMapState :: (Trans t, Trans s, Trans r') =>
                 (t -> Maybe r') -> [(t,s)] -> [(Maybe (r',s),[t])]
eagerMapState _ [] = [(Nothing, [])]
eagerMapState f xs = zipWith (\x y -> (lazySelfState f x y, [])) (repeat Working) xs
\end{bad}
\begin{code}
lazyMapState :: (Trans t, Trans r') =>
                (t -> Maybe r') -> [(t, State)] -> [(Maybe (r', State),[t])]
lazyMapState _ [] = trace "lazyMapState: got Nil in input" $ 
                    []
lazyMapState _ ((_, Abort):_) = trace "lazyMapState: got Abort in input" $ 
                                []
lazyMapState f ((t, Working):xs) = let res = lazySelfStateM f t Working
                                       rest = case res of
                                                Just _ -> 
                                                    trace "lazyMapState: result was Just, continue" $
                                                    lazyMapState f xs
                                                Nothing -> 
                                                    trace "lazyMapState: got Nothing, aborting..." $
                                                    []
                                   in (res, []):rest
-- lazyMapState _ _ = trace "lazyMapState: got something weird in input" $
--                   [] -- ok
\end{code}
\begin{skeleton}
--Stateful Version without DM
mwRingSLF' :: (Trans t, Trans r, Trans s, NFData r') =>
       [Int] -> Int ->                         --Nesting, amount of Workers
       ([(t,s)] -> [(Maybe (r',s),[t])]) ->    --wf
       ([Maybe (r',s)] -> s -> [r]) ->         --result transformation
       ([[r]] -> [r]) ->                       --result merge
       ([t]->[t]->s->[t]) ->                   --Taskpool Transform Attach
-- !! Split and Detatch policy has to give tasks away, unless all tasks are cut
       ([t]->s->([t],[t])) ->                  --Split Policy (remote Req)
       ([t]->s->([t],Maybe (t,s))) ->          --tt Detach (local Req)
       (s->s->Bool) ->            --Checks if new State is better than old State
       s -> [t] -> ([r],Int)                   --initState, tasks, results
\end{skeleton}
\begin{code}
stateIter :: forall bigTask task res bigRes.
            ((Trans bigTask, Trans task, 
              Trans res, Trans bigRes, 
              NFData res, Show res) =>
             (bigTask -> [task]) -> 
             (task -> Maybe res) -> 
             ([res] -> bigRes) -> 
             bigTask -> bigRes)
stateIter createTasks wf combine task 
    = let (ress, n) = trace "stateIter: calling internal skeleton... " $ 
                      mwRingSLF' [] noPe f 
                      resTran concat tta split detach (<=) Working tasks
      in combine ress
          where tasks :: [task]
                tasks = -- trace "stateIter: calling createTasks" $ 
                        createTasks task
                -- convertTask :: Trans t => [t] -> [(t, State)]
                -- convertTask xs = zip xs $ repeat Working
                f :: [(task, State)] -> [(Maybe (res, State),[task])]
                f = -- trace "stateIter: calling converted wf" $ 
                    lazyMapState wf
                resTran :: [Maybe (res, State)] -> State -> [res]
                resTran xs s = -- trace "stateIter: transforming result" $ 
                               fst $ lazyMapMaybeState g xs s
                    where g :: Maybe (res, State) -> Maybe res
                          g (Just (x, Working)) = trace "g of resTran: all green" $ 
                                                  Just x
                          g x = trace ("g of resTran: got something bad: " 
                                      ++ show x)
                                $ Nothing                          
                tta xs ys Working = trace "tta: all green" $ 
                                    xs ++ ys
                tta _ _ Abort = trace "tta: got Abort!" $ 
                                []
                -- tta _ _ _ = trace "tta: OOPS!" $ error "tta" -- ok
                split [] Working = trace "split: empty list!" $
                                   ([], [])
                split xs Working = trace "split: all green" $ 
                                   splitAt (length xs `div` 2) xs
                split _ _ = trace "split: got Abort!" $
                            ([], [])
                detach (x:xs) Working = trace "detach: all green" $ 
                                        (xs, Just (x, Working))
                detach _ _ = trace "detach: got Abort!" $ 
                             ([], Nothing)
\end{code}