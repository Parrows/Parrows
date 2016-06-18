Parrows
=======

This project aims to provide an interface for parallel computation
using arrows.

Type classes
------------

The only new Typeclass is the ParallelSpawn typeclass:

```haskell
class (Arrow arr) => ParallelSpawn arr where
    parEvalN :: (NFData b) => arr (Parrow arr a b) (arr [a] [b])
```

where parEvalN creates a new arrow that evaluates all arrows the Parrow consists of (type synonym for a list of arrows) in parallel.

Additionally, these functions are supported:

```haskell
-- evaluate the Parrow in parallel chunks (works on infinite lists)
parEvalNLazy :: (ParallelSpawn arr, ArrowChoice arr, ArrowApply arr, NFData b) => arr (Parrow arr a b, Int) (arr [a] [b])

-- evaluate the two arrows in parallel
parEval2 :: (ParallelSpawn arr, ArrowApply arr, NFData b, NFData d) => arr (arr a b, arr c d) (arr (a, c) (b, d))
```

The Multicore and ParMonad implementations currently have this implemented for all Arrows with ArrowApply and ArrowChoice.

Currently supported parallel skeletons
--------------------------------------

parZipWith:

```haskell
parZipWith :: (ParallelSpawn arr, ArrowApply arr, NFData c) => arr (arr (a, b) c, ([a], [b])) [c]
```

parMap:

```haskell
parMap :: (ParallelSpawn arr, ArrowApply arr, NFData b) => arr (arr a b, [a]) [b]
```


Currently supported modes:
-------------------------

- Multicore Haskell
- Par Monad

Modes yet to be implemented:

- Eden
- LVish https://hackage.haskell.org/package/lvish
- ...