module ParallelSplit.ParMonad where

import ParallelSplit.Definition
import Control.Monad.Par (runPar, spawnP, get)
import Control.Parallel.Strategies
import Data.List.Split

-- do we use spawn or spawnP here?
-- how does the strictness concern us here?

{-instance ParallelSplit ParKleisli where
    (<||>) f g = P $ \a -> PR $ runPar $ do y1 <- spawn_ (return (evalKleisli f a))
                                            y2 <- spawn_ (return (evalKleisli g a))
                                            b  <- get y1
                                            c  <- get y2
                                            return (b, c)
    (<&&>) (P f) mergefn = P $ \a -> let (PR bc) = f a
                                     in PR $ uncurry mergefn bc
    (<|||>) f g = P $ \(a, c) -> PR $ runPar $ do y1 <- spawn_ (return (evalKleisli f a))
                                                  y2 <- spawn_ (return (evalKleisli g c))
                                                  b  <- get y1
                                                  d  <- get y2
                                                  return (b, d)
    (<&&&>) (P f) mergefn = P $ \ac -> let (PR bd) = f ac
                                       in PR $ uncurry mergefn bd
-}

spawn' f x = do y <- spawnP (f x)
                get y

instance ParallelSpawn (->) where
    spawn (Parrow fs) = \as -> runPar $ sequence (zipWith (\f a -> f a) (map spawn' fs) as)

