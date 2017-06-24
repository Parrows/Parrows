We need to lift integer division functions |div| and |mod| to multiple-residue integer class. We do not need an instance of |Integral| for this, as we can modd our application. But anyway we build a homebrew instance of |Integral|, as we need to deal with both a single residue and a list of them.

So |Integral| here is Prelude's |Integral|, but |Integral'| is our own. It's essentially the same, but without this weird dependencies.

\begin{code}
{-# OPTIONS -XTypeSynonymInstances #-}
module IntMultiLift where

import MathObj.Residue.IntMulti
import MathObj.Residue.Modulo
import qualified Prelude as P
import Prelude (fst, snd, otherwise, error, (/=), (.), ($), Integral, zipWith, unzip, Int, Integer)
\end{code}

First we define |divMod| for a single residue
\begin{code}
divModSingle :: Integral a => Mod a -> Mod a -> (Mod a, Mod a)
divModSingle (Z a m) (Z b m') | m/=m' = error "divMod: different residues"
                              | otherwise =
                                  let (q, r) = P.divMod a b
                                  in (makeZ q m, makeZ r m)
\end{code}

Type class time!
\begin{code}
class Integral' a where
    divMod :: a -> a -> (a, a)
    div :: a -> a -> a
    div x y = fst $ divMod x y
    mod :: a -> a -> a
    mod x y = snd $ divMod x y
\end{code}

Single residue instance:
\begin{code}
instance Integral a => Integral' (Mod a) where
    divMod = divModSingle
\end{code}

Multiple residue instance, just apply a single residue instance to each element.
\begin{code}
instance Integral a => Integral' (IMods a) where
    divMod x y = unzip $ zipWith divModSingle x y
\end{code}

Integer intances:
\begin{code}
instance Integral' Int where
    divMod = P.divMod

instance Integral' Integer where
    divMod = P.divMod
\end{code}
