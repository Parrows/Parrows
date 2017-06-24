{#- -XMultiParamTypeClasses -XFunctionalDependencies -#}
Typklasse fuer Vektoren.

Achtung! Wir sind hier eine Instanz von Num. Das ist dafuer da, um Vektoren von Vektoren transparent darstellen zu koennen.

\begin{code}
module MathObj.Vector.Base where

import Prelude hiding (divMod)
{-
class Num a => Vectors a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    divMod :: a -> a -> (a,a)
    length :: a -> Int
-}
class Num x => Vectors v x | v -> x where
    l :: (x -> x -> x) -> v -> v -> v
    length :: v -> Int
\end{code}

\begin{nocode}
div = fst divMod
mod = snd divMod
divMod a b = (a `div` b, a `mod` b)
\end{nocode}
