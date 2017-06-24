Vulgar Fractions.

This is -*- Literate Haskell -*_
\begin{code}
module MathObj.Generic.VulgarFraction where

import Data.List (partition) -- for read
\end{code}

We separate our fraction parts with \

\begin{code}
data Fraction a = F a a
                  deriving (Eq, Ord, Show, Read)

nom :: Fraction a -> a
nom (F x _) = x
denom :: Fraction a -> a
denom (F _ y) = y
infix 8 /:/
x /:/ y = let g = gcd x y
              a = x`div`g
              b = y`div`g
          in F a b

renorm (F x y) = x /:/ y
\end{code}
\begin{zzzcode}
instance (Show a, Num a, Ord a) => Show (Fraction a) where
    show (F x y) | y == 1 = show x
                 | y == -1 = show (-x)
                 | x < 0 && y < 0 = show $ F (-x) (-y)
                 | otherwise = show x ++ "/" ++ show y
\end{zcode}
\begin{zcode}
instance (Read a) => Read (Fraction a) where
    read input | elem '/' input = let (nom, denom) = partition (=='/') input
                                      a = read nom
                                      b = read denom
                                  in a /:/ b
               | otherwise = error "Can't read fraction!"
\end{zcode}
\begin{zcode}
instance (Read a, Integral a) => Read (Fraction a) where
    readsPrec _ s = readsFrac s

readsFrac :: (Integral a, Read a) => ReadS (Fraction a)
readsFrac s | elem '/' s = let (nom, r) = partition (=='/') s
			       a = read nom
			       b = read denom
			       l = lex r
			       (denom, rest) | length l > 0 = head l
				             | otherwise = ("1", "")
			   in [(a/:/b, rest)]
	    | otherwise = []
\end{zcode}
Die Arithmetik.
a   c    ad + bc
- + - =  -------
b   d      bd

a   c    ac
- * - =  --
b   d    bd
\begin{code}
add (F a b) (F c d) = (a*d + b*c)/:/(b*d)
substr x y = add x $ neg y
mult (F a b) (F c d) = (a*c)/:/(b*d)
divide x y = mult x $ inverse y
neg (F a b) = F (-a) b
inverse (F a b) = F b a
evalf (F a b) = let x = fromIntegral a
                    y = fromIntegral b
                in x/y

instance (Num a, Integral a) => Num (Fraction a) where
    (+) = add
    (-) = substr
    (*) = mult
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a>0 && b>0 = 1
                   | a<0 && b>0 = -1
                   | a<0 && b<0 = 1
                   | a>0 && b<0 = -1
                   | otherwise = 0
    fromInteger x = (fromIntegral x)/:/1
instance (Num a, Integral a) => Fractional (Fraction a) where
    (/) = divide
    fromRational = undefined
\end{code}

A maximum of a special art
\begin{code}
maxFarey (F a b) = max (abs a) (abs b)
\end{code}
