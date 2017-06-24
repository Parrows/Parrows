This is -*- Literate Haskell -*-

/!\ Hier wird es nicht nach Subtests in Rabin-Miller Test parallelisiert,
    sondern nach den einzenen Tests! Das ist imkompatibel zu dem Rest!

Diese Datei testet eine Arbeitsverteilung. Im Wesentlichen:

Wir haben eine Workerfunktion und Satz von Eingabedaten. Auf jeden Datensatz wird eine Workerfunktion parallel angesetzt. Wenn (in diesem Fall: einer der) Worker Erfolg meldet, dann sind wir fertig. Ansonsten wenden wir die Arbeitsfunktion auf den naechsten Satz von Eingabedaten.

In diesem Fall ist die Workerfunkionen die Suche nach starken Primzahlkandidaten.

Typen:

data:
        hier ein Integer. Ungerade.
        ab den suchen wir die Primzahlkandidaten.
        Wird noch mit Int fuer "Wie viele Tasks wir gerade erstellt haben" versehen.

task:   ein Integer. Ein Primzahlkandidat.

(data -> [task]):
         iterate (+2) data
         Ist ein undendlicher Strom an Tasks gueltig?

subRes:  Maybe Integer.
         Ist Nothing, falls das nichts war und
         Just Integer, falls wir ein passenden Kandidaten gefunden haben.

(task -> subRes):
         Rabin-Miller Test.

([subRes] -> Either result [task]):
         liefert entweder den gefundenen Kandidaten oder erstellt neue Tasks.
         Muss wissen, wie viele Tasks hat urspruenglich der Taskgenerator erstellt und data entsprechend zu inkeremntieren?

\begin{code}
{-# OPTIONS -cpp #-}
#ifdef INTERACTIVE
module Test (.., rabinMiller) where
#else
module Main where
#endif

import Maybe
import RabinMiller (rabinMiller)      -- Workerfunktion
import IterUntil (iterUntil)          -- Iterationsskelett
-- import VanillaUntil (iterUntil)

import System (getArgs)
import Debug.Trace (trace)
-- import Text.Printf (printf)
-- trace _ = id
\end{code}

Die Unterfunktionen.
\begin{code}
generateTasks :: (Int, Integer) -> [Integer]
generateTasks (k, n) | odd n = trace "Generating tasks (odd)..." $
                               take k $ iterate (+2) n
                     | even n = trace "Generating tasks (even)..." $
                                take k $ iterate (+2) (n+1)
                     | otherwise = error "generateTasks"

doneContinue :: (Int, Integer) -> [Maybe Integer] -> Either Integer ([Integer], (Int, Integer))
doneContinue (k, n) cands = let finalResults = catMaybes cands
                                -- n' = n + k
                                n' = (+) n $ fromIntegral k
                                newTasks = generateTasks (k, n')
                            in if (not $ null finalResults)
                               then Left (head finalResults)
                               else Right (newTasks, (k, n'))

worker _ task = trace ("Computing task " ++ show' task) (rabinMiller task, ())
\end{code}
iterUntil :: (Trans local, Trans task, 
              Trans subResult) => 
             localM ->
             [local] ->
             dataIn ->
             (dataIn -> [task]) ->
             (local -> task -> (subResult,local)) ->
             (localM -> [subResult] -> Either result ([task],localM)) ->
             result
\begin{code}
doIt k n = trace "Starting the skeleton..." $
           iterUntil (k, n) (repeat ()) (k, n) generateTasks worker doneContinue
\end{code}

\begin{code}
main = do
  args <- getArgs
  let k = read $ args!!0
      m = read $ args!!1
      -- n = 2^m-1
      n = m
      result = doIt k n
  putStrLn $ "Launching " ++ show k ++ " tasks for " ++ show' n
  putStrLn $ show result
  putStrLn "done"
\end{code}

not showing long numbers
\begin{code}
show' :: Integer -> String
show' n | k<11 = s
        | otherwise = take 4 s ++ "..." ++ drop (k-4) s
        where k = length s
              s = show n
\end{code}
