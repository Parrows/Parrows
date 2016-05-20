module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies

instance ParallelSplit ParKleisli where
    -- do this with the par monad so we can have this type in parallel
    (<||>) f g = P $ \a -> PR $ let b = evalKleisli f a
                                    c = evalKleisli g a
                                in
                                    b `par` c `pseq` (b, c)
    (<&&>) (P f) mergefn = P $ \a -> let (PR bc) = f a
                                     in PR $ uncurry mergefn bc
    -- do this with the par monad so we can have this type in parallel
    (<|||>) f g = P $ \(a, c) -> PR $ let b = evalKleisli f a
                                          d = evalKleisli g c
                                      in
                                          b `par` d `pseq` (b, d)
    (<&&&>) (P f) mergefn = P $ \ac -> let (PR bd) = f ac
                                       in PR $ uncurry mergefn bd
    -- foldr1 is probably not the best choice for this?
    liftToParMap f = P $ \as -> parMapPR f as
    reduce (P f) mergefn = P $ \as -> let (PR bs) = f as
                                      in PR $ foldr1 mergefn bs

instance ParallelSplit (->) where
    (<||>) f g = \a -> let b = f a
                           c = g a
                       in
                           b `par` c `pseq` (b, c)
    (<&&>) f mergefn = \a -> let bc = f a
                             in uncurry mergefn bc
  -- do this with the par monad so we can have this type in parallel
    (<|||>) f g = \(a, c) -> let b = f a
                                 d = g c
                             in
                                 b `par` d `pseq` (b, d)
    (<&&&>) f mergefn = \ac -> let bd = f ac
                               in uncurry mergefn bd
    -- foldr1 is probably not the best choice for this?
    liftToParMap = parMap rdeepseq
    reduce f mergefn = \as -> foldr1 mergefn (f as)

parMapPR :: ParKleisli a b -> [a] -> ParRes [b]
parMapPR (P f) = sequence . fmap f

unwrapKleisli :: ParKleisli a b -> (a -> ParRes b)
unwrapKleisli (P f) = f

unwrapParRes :: ParRes a -> a
unwrapParRes (PR a) = a

evalKleisli :: ParKleisli a b -> a -> b
evalKleisli fn a = unwrapParRes $ unwrapKleisli fn a
