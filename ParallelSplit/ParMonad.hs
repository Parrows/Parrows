module ParallelSplit.ParMonad where

import ParallelSplit.Definition
import Control.Monad.Par (runPar, spawn_, get)
import Control.Parallel.Strategies
import Data.List.Split

-- do we use spawn or spawnP here?
-- how does the strictness concern us here?

instance ParallelSplit ParKleisli where
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

instance ParallelSplit (->) where
    (<||>) f g = \a -> runPar $ do y1 <- spawn_ (return (f a))
                                   y2 <- spawn_ (return (g a))
                                   b  <- get y1
                                   c  <- get y2
                                   return (b, c)
    (<&&>) f mergefn = \a -> let bc = f a
                             in uncurry mergefn bc
    (<|||>) f g = \(a, c) -> runPar $ do y1 <- spawn_ (return (f a))
                                         y2 <- spawn_ (return (g c))
                                         b  <- get y1
                                         d  <- get y2
                                         return (b, d)
    (<&&&>) f mergefn = \ac -> let bd = f ac
                               in uncurry mergefn bd

chunkCount len threadcnt
    | threadcnt > len = 1
    | otherwise = len `div` threadcnt

parMap :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap threadcnt f as = go f (chunksOf (chunkCount (length as) threadcnt) as)
                        where
                            go f [] = []
                            go f [as] = map f as
                            go f (as:rest) = (map f <|||> go f <&&&> (++)) (as, rest)

unwrapKleisli :: ParKleisli a b -> a -> ParRes b
unwrapKleisli (P f) = f

unwrapParRes :: ParRes a -> a
unwrapParRes (PR a) = a

evalKleisli :: ParKleisli a b -> a -> b
evalKleisli fn a = unwrapParRes $ unwrapKleisli fn a
