import Control.Monad.Par (runPar, spawnP, get)
import Control.Parallel.Strategies

-- basic arrows
class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  --first :: a b c -> a (b,d) (c,d)

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)

newtype Kleisli m a b = K (a -> m b)

instance Monad m => Arrow (Kleisli m) where
  arr f = K $ return . f
  K f >>> K g = K (\b -> f b >>= g)

-- now for parallel stuff
data ParRes a = PR a
                deriving Show

-- use the real Par monad or Eden's low-level interface or even Concurrent Haskell with Monad == IO here
-- maybe express it and ParKleisli in terms of Kleisli?

-- boilerplate begins
instance Monad ParRes where
    return = PR
    PR x >>= f = f x

instance Functor ParRes where
    fmap f (PR x) = PR $ f x

instance Applicative ParRes where
    -- PR (a -> b) <*> PR a
    PR f <*> PR x = PR (f x)
    pure = PR
-- boilerplate ends

newtype ParKleisli a b = P (a -> ParRes b)
-- same Kleisli type as above, so:
instance Arrow (ParKleisli) where
  arr f = P $ return . f
  P f >>> P g = P (\b -> f b >>= g)

parMapPR :: ParKleisli a b -> [a] -> ParRes [b]
parMapPR (P f) = sequence . fmap f

-- using split <|||> and merge <&&&> basically every computation
-- can be parallelized but if the single tasks are already computed
-- quite fast, the overhead for splitting and merging
-- is probably too high (regarding both computationally and
-- code length/complexity due to the types ending up being something like
-- ((,),(,)), etc. if merges dont happen)

-- this could be broken down for arrows so we can use this kind
-- of parallelism with normal functions

class (Arrow arr) => ParallelSplit arr where
    (<||>) :: (NFData b, NFData c) => arr a b -> arr a c -> arr a (b, c)
    (<&&>) :: arr a (b, c) -> (b -> c -> d) -> arr a d

    (<|||>) :: (NFData b, NFData d) => arr a b -> arr c d -> arr (a, c) (b, d)
    (<&&&>) :: arr (a, c) (b, d) -> (b -> d -> e) -> arr (a, c) e

    liftToParMap :: (NFData b) => arr a b -> arr [a] [b]
    reduce :: arr [a] [b] -> (b -> b -> b) -> arr [a] b

-- TODO: remove duplicated code

instance ParallelSplit ParKleisli where
    -- do this with the par monad so we can have this type in parallel
    (<||>) f g = P $ \a -> PR $ runPar $ do y1 <- spawnP (evalKleisli f a)
                                            y2 <- spawnP (evalKleisli g a)
                                            b  <- get y1
                                            c  <- get y2
                                            return (b, c)
    (<&&>) (P f) mergefn = P $ \a -> let (PR bc) = f a
                                     in PR $ uncurry mergefn bc
    -- do this with the par monad so we can have this type in parallel
    (<|||>) f g = P $ \(a, c) -> PR $ runPar $ do y1 <- spawnP (evalKleisli f a)
                                                  y2 <- spawnP (evalKleisli g c)
                                                  b  <- get y1
                                                  d  <- get y2
                                                  return (b, d)
    (<&&&>) (P f) mergefn = P $ \ac -> let (PR bd) = f ac
                                       in PR $ uncurry mergefn bd
    -- foldr1 is probably not the best choice for this?
    liftToParMap f = P $ \as -> parMapPR f as
    reduce (P f) mergefn = P $ \as -> let (PR bs) = f as
                                      in PR $ foldr1 mergefn bs

instance ParallelSplit (->) where
    (<||>) f g = \a -> runPar $ do y1 <- spawnP (f a)
                                   y2 <- spawnP (g a)
                                   b  <- get y1
                                   c  <- get y2
                                   return (b, c)
    (<&&>) f mergefn = \a -> let bc = f a
                             in uncurry mergefn bc
  -- do this with the par monad so we can have this type in parallel
    (<|||>) f g = \(a, c) -> runPar $ do y1 <- spawnP (f a)
                                         y2 <- spawnP (g c)
                                         b  <- get y1
                                         d  <- get y2
                                         return (b, d)
    (<&&&>) f mergefn = \ac -> let bd = f ac
                               in uncurry mergefn bd
    -- foldr1 is probably not the best choice for this?
    liftToParMap f = \as -> parMap rdeepseq f as
    reduce f mergefn = \as -> let bs = f as
                              in foldr1 mergefn bs


unwrapKleisli :: ParKleisli a b -> (a -> ParRes b)
unwrapKleisli (P f) = f

unwrapParRes :: ParRes a -> a
unwrapParRes (PR a) = a

evalKleisli :: ParKleisli a b -> a -> b
evalKleisli fn a = unwrapParRes $ unwrapKleisli fn a

-- idea lazily copied from:
-- https://gist.github.com/morae/8494016

fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge fn (x:xs) (y:ys)
      | fn x y == LT = x:merge fn xs (y:ys)
      | fn x y == EQ = x:merge fn xs (y:ys)
      | otherwise = y:merge fn (x:xs) ys

-- this is our multithreaded function
-- it spawns 2 threads that do the mergesort
mergesortPar :: (NFData a) => (a -> a -> Ordering) -> [a] -> [a]
mergesortPar fn xs
  | length xs <= 1 = xs
  -- we get multithreading for zero readability cost here
  -- first we split the sorting process into two different processes <|||>
  -- and then we merge them with <&&&>
  | otherwise = (mergesort fn <|||> mergesort fn <&&&> merge fn) (fsthalf xs, sndhalf xs)

mergesort :: (a -> a -> Ordering) -> [a] -> [a]
mergesort fn xs
    | length xs <= 1 = xs
    | otherwise = merge fn (mergesort fn (fsthalf xs)) (mergesort fn (sndhalf xs))

stuff :: [Integer]
stuff = [1..999999]


order x y
    | x > y = GT
    | x < y = LT
    | x == y = EQ
