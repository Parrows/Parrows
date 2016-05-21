module ParallelSplit.Multicore where

import ParallelSplit.Definition

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split

-- investigate the different evaluation Strategies
-- if we use this kind of parallelism

instance ParallelSplit ParKleisli where
    (<||=>) f g = P $ \a -> PR $ let b = evalKleisli f a
                                     c = evalKleisli g a
                                 in
                                     b `par` c `pseq` [b, c]
    (<&&=>) (P f) mergefn = P $ \as -> let (PR bs) = f as
                                       in PR $ foldr1 mergefn bs

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

instance ParallelSplit (->) where
    (<||=>) f g = \a -> let b1 = f a
                            b2 = g a
                        in
                            b1 `par` b2 `pseq` [b1, b2]
    (<&&=>) f mergefn = \as -> let bs = f as
                             in foldr1 mergefn bs
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
