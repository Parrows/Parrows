Klassisches 2-radix FFT, sequentiell, alte Quellen.
\begin{code}
module MathObj.FFT.Reference (fft, ifft) where

import Data.Complex
import Data.List
\end{code}

Export mit indexed Vectors:
\begin{code}
lift1 f (n, xs) = let res = f xs
                  in (length res, res)

fft = lift1 fft'
ifft = lift1 ifft'
\end{code}

Alte Quellen.
\begin{code}
-- fft_do :: [Complex Type] -> Complex Type -> [Complex Type]
fft_do :: RealFloat a => [Complex a] -> Complex a -> [Complex a]
fft_do [k] w = [k]
fft_do f   w = mix (fft_do (l @+ r) (w*w)) (fft_do ((l @- r)@* aw) (w*w))
    where (l, r) = split f
          n  = length f
          aw = take n $ iterate (*w) 1

fft' :: RealFloat a => [Complex a] -> [Complex a]
fft' f = fft_do f (root_of_unity 1 (length f))

ifft' :: RealFloat a => [Complex a] -> [Complex a]
ifft' f = reverse g /$ (fromIntegral $ length g)
    where g = fft_do f w
          w = root_of_unity (-1) (length f)

-- possible limitation due to length of mantissa in realfloat
root_of_unity :: (RealFloat a, Integral b) => b -> b -> Complex a
root_of_unity j n = cos (2*pi*j'/n') :+ sin (2*pi*j'/n')
    where j' = fromIntegral j
          n' = fromIntegral n
                    
lift :: (Num a, Integral a, RealFloat b) => [a] -> [Complex b]
lift = map (fromIntegral)

showC :: (RealFloat a, Integral b) => [Complex a] -> [b]
showC = map (round . realPart)

unlift = showC

-- @ is an "array", $ is a scalar. perl notation banzai!
infixr 6 @+
(@+) :: Num a => [a] -> [a] -> [a]
(@+) f g = zipWith (+) f g
infixr 6 @-
(@-) :: Num a => [a] -> [a] -> [a]
(@-) f g = zipWith (-) f g
infixr 7 @*
(@*) :: Num a => [a] -> [a] -> [a]
(@*) f g = zipWith (*) f g
infixr 7 /$
infixr 7 //$
-- (/$) :: (Num a, Num b, Fractional c) => [a] -> b -> [c]
(/$)  f n = map (/n) f
(//$) :: (Integral b) => [b] -> b -> [b]
(//$) f n = map (flip div $ fromIntegral n) f

-- Koefficient filter
kfilter :: (Int -> Bool) ->  [a] -> [a]
kfilter f xs = [xs!!i | i<-filter (f) [0 .. length xs - 1] ]

kodd, keven :: [a] -> [a]
-- kodd xs  [xs!!i | i<-filter (odd)  [0..length xs-1] ]
-- keven xs [xs!!i | i<-filter (even) [0..length xs-1] ]
kodd = kfilter odd
keven = kfilter even

-- TODO: rewrite!
-- takes 13% of whole time
mix' :: [a] -> [a] -> [a]
mix' []   ys   = ys
mix' xs   []   = xs
mix' (x:xs) (y:ys) = x:y:mix' xs ys
-- mix f   g  = kodd f ++ kodd g ++ keven f ++ keven g

-- this version is less complex in time, but more complex in space
mix :: [a] -> [a] -> [a]
mix f g = concat $ zipWith (\x y -> [x, y]) f g

-- ACHTUNG: andere semantik von pad!
pad :: (Num a) => [a] -> Int -> [a]
pad f n = (take (n-length f) $ repeat 0) ++ f
unpad :: (Num a, Eq a) => [a] -> [a]
unpad f = dropWhile (== 0) f


getNextPower2 :: Integral a => a -> a
getNextPower2 n = head $ dropWhile (<n) $ iterate (2*) 1
{-
fftmult :: Num a => [a] -> [a] -> [a]
fftmult f g = unpad $ (pad f n) %*% (pad g n)
    where n = getNextPower2 $ (length f) + (length g)

fftmult' f g n = unpad $ (pad f n) %*% (pad g n)
-}

padNextPower2 :: Num a => [a] -> [a]
padNextPower2 f = pad f (getNextPower2 $ length f)


meh f xs = f (n `div` 2) xs
    where n = length xs
left = meh take
right = meh drop

{-
split (n, xs) = ((m, ls), (m, rs))
    where m = n `div` 2
          (ls, rs) = splitAt m xs
-}
split xs = let m = (length xs) `div` 2
           in splitAt m xs

\end{code}