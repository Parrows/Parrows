{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ParallelSplit.ParMonad where

import ParallelSplit.Definition
import Control.Monad.Par
import Control.Arrow

zipWithArrM :: (ArrowApply arr, ArrowChoice arr, Applicative m) => (arr (a, b) (m c)) -> arr ([a], [b]) (m [c])
zipWithArrM f = (arr $ \abs -> (zipWithArr f, abs)) >>> app >>> arr sequenceA

parEval' :: (ArrowApply arr, ArrowChoice arr, NFData b) => arr ([arr a b], [a]) (Par [b])
parEval' = (arr $ \(fs, as) ->
                    (zipWithArrM (app >>> arr return >>> arr Control.Monad.Par.spawn), (fs, as)))
             >>> app >>> arr (>>= \ibs -> mapM get ibs)

instance (ArrowApply arr, ArrowChoice arr) => ParallelSpawn arr where
    parEvalN = (arr $ \fs -> ((arr $ \as -> (parEval', (fs, as))) >>> app >>> arr runPar))