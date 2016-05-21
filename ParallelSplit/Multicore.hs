module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split
import Debug.Trace

-- investigate the different evaluation Strategies
-- if we use this kind of parallelism

{-instance ParallelSplit ParKleisli where
    (<||>) f g = P $ \a -> PR $ let b = evalKleisli f a
                                    c = evalKleisli g a
                                in
                                    b `par` c `pseq` (b, c)
    (<&&>) (P f) mergefn = P $ \a -> let (PR bc) = f a
                                     in PR $ uncurry mergefn bc
    (<|||>) f g = P $ \(a, c) -> PR $ let b = evalKleisli f a
                                          d = evalKleisli g c
                                      in
                                          b `par` d `pseq` (b, d)
    (<&&&>) (P f) mergefn = P $ \ac -> let (PR bd) = f ac
                                       in PR $ uncurry mergefn bd
-}

instance ParallelSplit (->) where
    (<|||=>) f g = \as -> let b1 = f (as !! 0)
                              b2 = g (as !! 1)
                          in
                              b1 `par` b2 `pseq` [b1, b2]
    (<|||==>) f g = \(fst:rest) -> let b1 = f fst
                                       b2 = g rest
                                   in
                                       b1 `par` b2 `pseq` b1 : b2

    (<||>) f g = \a -> let b = f a
                           c = g a
                       in
                           b `par` c `pseq` (b, c)
    (<&&>) f mergefn = \a -> let bc = f a
                             in uncurry mergefn bc
    (<|||>) f g = \(a, c) -> let b = f a
                                 d = g c
                             in
                                 b `par` d `pseq` (b, d)
    (<&&&>) f mergefn = \ac -> let bd = f ac
                               in uncurry mergefn bd

parMap :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap threadcnt f as = ((chunky (min threadcnt (length as)) (map f)) <&&&=> (++)) (chunksOf chkln as)
                        where chkln = (chunkLen (length as) threadcnt)

unwrapKleisli :: ParKleisli a b -> a -> ParRes b
unwrapKleisli (P f) = f

unwrapParRes :: ParRes a -> a
unwrapParRes (PR a) = a

evalKleisli :: ParKleisli a b -> a -> b
evalKleisli fn a = unwrapParRes $ unwrapKleisli fn a
