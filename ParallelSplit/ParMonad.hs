module ParallelSplit.ParMonad where

import ParallelSplit.Definition
import Control.Monad.Par (runPar, spawnP, get)
import Control.Parallel.Strategies

-- do we use spawn or spawnP here?
-- how does the strictness concern us here?

instance ParallelSplit ParKleisli where
    (<||=>) f g = P $ \a -> PR $ runPar $ do y1 <- spawnP (evalKleisli f a)
                                             y2 <- spawnP (evalKleisli g a)
                                             b1  <- get y1
                                             b2  <- get y2
                                             return [b1, b2]
    (<&&=>) (P f) mergefn = P $ \a -> let (PR b) = f a
                                      in PR $ foldr1 mergefn b

    (<||>) f g = P $ \a -> PR $ runPar $ do y1 <- spawnP (evalKleisli f a)
                                            y2 <- spawnP (evalKleisli g a)
                                            b  <- get y1
                                            c  <- get y2
                                            return (b, c)
    (<&&>) (P f) mergefn = P $ \a -> let (PR bc) = f a
                                     in PR $ uncurry mergefn bc
    (<|||>) f g = P $ \(a, c) -> PR $ runPar $ do y1 <- spawnP (evalKleisli f a)
                                                  y2 <- spawnP (evalKleisli g c)
                                                  b  <- get y1
                                                  d  <- get y2
                                                  return (b, d)
    (<&&&>) (P f) mergefn = P $ \ac -> let (PR bd) = f ac
                                       in PR $ uncurry mergefn bd

instance ParallelSplit (->) where
    (<||=>) f g = \a -> runPar $ do y1 <- spawnP (f a)
                                    y2 <- spawnP (g a)
                                    b1  <- get y1
                                    b2  <- get y2
                                    return [b1, b2]
    (<&&=>) f mergefn = \a -> foldr1 mergefn (f a)

    (<||>) f g = \a -> runPar $ do y1 <- spawnP (f a)
                                   y2 <- spawnP (g a)
                                   b  <- get y1
                                   c  <- get y2
                                   return (b, c)
    (<&&>) f mergefn = \a -> let bc = f a
                             in uncurry mergefn bc
    (<|||>) f g = \(a, c) -> runPar $ do y1 <- spawnP (f a)
                                         y2 <- spawnP (g c)
                                         b  <- get y1
                                         d  <- get y2
                                         return (b, d)
    (<&&&>) f mergefn = \ac -> let bd = f ac
                               in uncurry mergefn bd

parMapPR :: Strategy b -> ParKleisli a b -> [a] -> ParRes [b]
parMapPR strategy (P f) = sequence . parMap (\(PR x) -> return (PR (x `using` strategy))) f

unwrapKleisli :: ParKleisli a b -> a -> ParRes b
unwrapKleisli (P f) = f

unwrapParRes :: ParRes a -> a
unwrapParRes (PR a) = a

evalKleisli :: ParKleisli a b -> a -> b
evalKleisli fn a = unwrapParRes $ unwrapKleisli fn a
