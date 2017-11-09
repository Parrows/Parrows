Vanilla iterUntil.

\begin{code}
module FarmUntil where

import Data.Maybe
import Control.Parallel.Eden (noPe, Trans, process)
import Control.Parallel.Eden.Map (farmS)
import Control.Parallel.Eden.Workpool (workpoolSortedNonBlock)

import Data.List (intersect, nub)

import Debug.Trace

map_farm, map_wp :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
map_farm = farmS noPe
map_wp = workpoolSortedNonBlock noPe 2
\end{code}

Wenn das Ergebnis nicht gebraucht wird, dann wird alles gekillt, inklusive die Kinder, die es produzieren.
\begin{code}
farmUntilBool :: Trans a => (a -> Bool) -> [a] -> Bool
farmUntilBool f xs = and $ map_farm f xs
\end{code}
\begin{code}
farmUntilMaybe :: (Trans a, Trans b) => (a -> Maybe b) -> [a] -> [b]
farmUntilMaybe f xs = let rs = map_farm f xs -- :: [Maybe b]
                          magic = takeWhile (\x -> case x of 
                                                     (Just a) -> True
                                                     _ -> False
                                            )
                      in catMaybes $ magic rs
                      -- after magic we have only Justs
\end{code}
\begin{code}
farmUntilMaybePass :: (Trans a, Trans b) => (a -> Maybe b) -> [a] -> [Maybe b]
farmUntilMaybePass f xs 
    = let rs = map_farm f xs -- :: [Maybe b]
          magic' = takeWhile (\x -> case x of 
                                      (Just a) -> True
                                      _ -> False
                             )
          magic = id
      in magic rs
      -- after magic we have only Justs
\end{code}

\begin{code}
mwUntilBool :: Trans a => (a -> Bool) -> [a] -> Bool
mwUntilBool f xs = and $ map_wp f xs
\end{code}

A more generic farmUntil.
\begin{code}
mapUntil :: (Trans a, Trans b, Trans c) =>
            ((a -> b) -> [a] -> [b]) ->    -- ^ map
            ([b] -> c) ->                  -- ^ reduce
            (a -> b) ->                    -- ^ worker function
            [a] -> c
mapUntil amap areduce f xs = areduce $ amap f xs -- meh.
\end{code}

Implement farmUntilBool with it:
\begin{code}
farmUntilBool' :: Trans a => (a -> Bool) -> [a] -> Bool
farmUntilBool' = mapUntil map_farm and
\end{code}

Code from JacobiSum:
\begin{jcode}
unify :: (Eq a, Ord a, Show a) => [[a]] -> [a]
unify = foldl1 intersect . nub -- nub gives better performance

unifyMaybe :: (Eq a, Ord a, Show a) => [Maybe [a]] -> Maybe [a]
unifyMaybe xss | hasNothing xss = trace "unifyMaybe: bailing out!" $ 
                                  Nothing
               | otherwise = Just $ unify $ catMaybes xss
    where hasNothing = not . all maybeBool
          maybeBool (Just _) = True
          maybeBool Nothing = False
jacobisumteststep3 0 xs n t lps = JS.jacobisumteststep3 xs n t lps

jacobisumteststep3 s xs n t lps
    = trace (unlines ["Tested total input " ++ show xs,
                      "Tested complete lp list " ++ show lps,
--                      "Result lp lists from all steps: " ++ show ress,
                      "Result lp list after reduce: " ++ show res]) $
      res
      where worker (p, q) = trace ("Testing " ++ show (p, q)) $
                            jacobisumteststep4 p k q n lps
                where k = nu (q-1) p  -- Vielfachheit von p in (q-1)
            ress | s==1 = map worker xs
                 | s==2 = map_par worker xs
                 | s==3 = map_farm worker xs
                 | s==4 = map_wp worker xs
                 | otherwise = error "Dunno such skeleton!"
            res = unifyMaybe ress
\end{jcode}

The same with our skeleton:
\begin{xcode}
jacobiWorker xs n t lps =
    let worker (p, q) = jacobisumteststep4 p k q n lps
          where k = nu (q-1) p  -- Vielfachheit von p in (q-1)
    in mapUntil map_wp unifyMaybe worker xs
\end{xcode}
