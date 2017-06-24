Listen ohne Laenge, unchunked.

\begin{code}
module MathObj.Vector.Naked where
import qualified Data.List as DL

-- wozu?
type Nacked a = ([a])

-- wir brauchen hier Typdefinition!
l2 :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
l2 (f) xs ys = zipWith f xs ys
l1 (f) xs = map (f) xs
length = DL.length
toList = id
fromList = id
transpose = DL.transpose
concat = DL.concat
(!!) = DL.(!!)
splitAt = DL.splitAt
filter = DL.filter
\end{code}

Um es bequemer zu machen:

\begin{code}
-- instance Num => Indexed where
--    div = l (P.div)
add = l2 (+)
substr = l2 (-)
mult = l2 (*)
divide = l2 (/)
absolute = l1 (abs)
divideSkalar xs k = divide xs (repeat k)
\end{code}

Hier erzeugen wir eine unendliche Liste und zipWith in l2 nimmt soviel, wie noetig ist.

Und die Kuerzel:
-- werden erst in Vektor.lhs definiert!
\begin{nocode}
(@+) = add
(@-) = substr
(@*) = mult
(@/) = divide
($/) = divideSkalar
\end{nocode}