Vanilla iterUntil for Eden-6.12

\begin{code}
module FarmUntil612 where

import Data.Maybe
import Control.Parallel.Eden (noPe, Trans, process)
import Control.Parallel.Eden.EdenSkel.MapSkels (map_farm, workpoolSortedNonBlock)
import Debug.Trace

-- need to define map-like workpool w/o tuning
map_wp :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
map_wp = workpoolSortedNonBlock noPe 3

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
