-- basic arrows
class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b,d) (c,d)

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


example1 =
    let f = P $ \x -> PR $ x+1
        g = P $ \x -> PR $ x*2
    in f >>> g
-- conditional, f before g

example2 =
      let f = arr (+1)
          g = arr (*2)
      in f <|||> g <&&&> (+)
-- f and g get evaluated in parallel and their results are then
-- merged into a single result with (+)
-- this can be particularly useful for two long running
-- tasks f and g that are independent of each others' results
--
-- by using this style (and by having the ParKleisli arrow)
-- we can easily represent _(simple) dependency graphs of computations
--
-- using split <|||> and merge <&&&> basically every computation
-- can be parallelized but if the single tasks are already computed
-- quite fast, the overhead for splitting and merging
-- is probably too high (regarding both computationally and
-- code length/complexity due to the types ending up being something like
-- (,,,) (,,,) if merges dont happen)

parMapPR :: ParKleisli a b -> [a] -> ParRes [b]
parMapPR (P f) = sequence . fmap f

parMap :: (a -> b) -> [a] -> [b]
parMap = map

-- this could be broken down for arrows so we can use this kind
-- of parallelism with normal functions

class (Arrow arr) => ParallelSplit arr where
    (<||>) :: arr a b -> arr a c -> arr a (b, c)
    (<&&>) :: arr a (b, c) -> (b -> c -> d) -> arr a d

    (<|||>) :: arr a b -> arr c d -> arr (a, c) (b, d)
    (<&&&>) :: arr (a, c) (b, d) -> (b -> d -> e) -> arr (a, c) e

    (<||||>) :: arr a b -> arr [a] [b]
    (<&&&&>) :: arr [a] [b] -> (b -> b -> b) -> arr [a] b

instance ParallelSplit ParKleisli where
    -- do this with the par monad so we can have this type in parallel
    (<||>) f g = P $ \a -> PR (evalKleisli f a, evalKleisli g a)
    (<&&>) (P f) mergefn = P $ \a -> let (PR bc) = f a
                                     in PR $ uncurry mergefn bc
    -- do this with the par monad so we can have this type in parallel
    (<|||>) f g = P $ \(a, b) -> PR (evalKleisli f a, evalKleisli g b)
    (<&&&>) (P f) mergefn = P $ \ac -> let (PR bd) = f ac
                                       in PR $ uncurry mergefn bd
    (<||||>) f = P $ \as -> parMapPR f as
    (<&&&&>) (P f) mergefn = P $ \as -> let (PR bs) = f as
                                        in PR $ foldr1 mergefn bs

instance ParallelSplit (->) where
    (<||>) f g = \a -> (f a, g a)
    (<&&>) f mergefn = \a -> let bc = f a
                             in uncurry mergefn bc
  -- do this with the par monad so we can have this type in parallel
    (<|||>) f g = \(a, b) -> (f a, g b)
    (<&&&>) f mergefn = \ac -> let bd = f ac
                               in uncurry mergefn bd
    (<||||>) f = \as -> parMap f as
    (<&&&&>) f mergefn = \as -> let bs = f as
                                in foldr1 mergefn bs


unwrapKleisli :: ParKleisli a b -> (a -> ParRes b)
unwrapKleisli (P f) = f

unwrapParRes :: ParRes a -> a
unwrapParRes (PR a) = a

evalKleisli :: ParKleisli a b -> a -> b
evalKleisli fn a = unwrapParRes $ unwrapKleisli fn a

main = do
    print ""
