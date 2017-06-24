Map matrix over a residue class and reconstruct it back.

This is -*- Literate Haskell -*_
\begin{code}
{-# OPTIONS -cpp -XParallelListComp #-}
module MathObj.Matrix.Residue where

import MathObj.Matrix.MatArr
import Data.Array

import MathObj.Generic.VulgarFraction
import MathObj.Primes.Determ
import MathObj.Residue.FracMulti

import Data.Array (Ix)
import Data.List (transpose)
import Data.Maybe

#define __EDEN__ 1
#ifdef __EDEN__
import Parallel.Eden (Trans, NFData, rnf)
import Parallel.SkelHelper
import Control.Parallel (pseq)
#endif

import Debug.Trace (trace)
\end{code}

Wir wollen [MatArr i (FSingleMod n)], bekommen aber MatArr i [FSingleMod n]!

\begin{xcode}
toResiduePrimes :: (Ix i, Integral n) => MatArr i (Fraction n) -> [n] -> MatArr i [FSingleMod n]
toResiduePrimes m ps = fmap (\x -> makeFZ x ps) m

toResidueLimit m l = let ps = primesFrom l
                     in toResiduePrimes m ps
\end{xcode}

Also wir muessen umkopieren!
\begin{code}
mapTranspose f m = let ms = elems m
                       cs = transpose $ map (f) ms
                       rs = map (listArray (bounds m)) cs
                   in rs

-- toResiduePrimes :: (Ix i, Integral n) => MatArr i (Fraction n) -> [n] -> [MatArr i (FSingleMod n)]
toResiduePrimes m ps -- = mapTranspose (flip makeFZ ps) m
    = trace ("mapping to " ++ (show $ length ps) ++ " primes") (mapTranspose (flip makeFZ ps) m)
\end{code}

\begin{xcode}
fromResidue :: (Ix i, Integral n) => MatArr i (FMods n) -> MatArr i (Fraction n)
fromResidue = fmap (restoreFZ)
fromResidueMaybe :: (Ix i, Integral n) => MatArr i (FMods n) -> MatArr i (Maybe (Fraction n))
fromResidueMaybe = fmap (restoreFZmaybe)
\end{xcode}

Für die parallele Version brauchen wir _Listen_ für die Übertragung!

\begin{code}
--type TransMat i n = (i, i, [[n]])
type TransMat i n = ((i, i), [[n]])
-- TransMat Int (Fraction n)
-- TransMat Int (FSingleMod n)
\end{code}
\begin{err}
fractionsMultimods :: (Ix i, Integral n) => TransMat i (Fraction n) -> [n] -> [TransMat i (FSingleMod n)]
fractionsMultimods (n, m, xs) ps = let yss = map (flip makeFZ ps) xs
		       	      	   in zip3 (repeat n) (repeat m) yss

multimodsMaybeFractions :: (Ix i, Integral n) => [TransMat i (FSingleMod n)] -> TransMat i (Maybe (Fraction n))
multimodsMaybeFractions xs = let ys = transpose $ map (\(_, _, x) -> x) xs
			     	 zs = map (restoreFZmaybe) ys
				 (n, m, _) = head xs
			     in (n, m, zs)

liftArrays :: (Ix i, Integral i, Integral n) => (MatArr i (FSingleMod n) -> MatArr i (FSingleMod n)) -> TransMat i (FSingleMod n) -> TransMat i (FSingleMod n)
liftArrays f (n, m, xs) = let arr = listArray ((1, 1), (n, m)) xs
	     	    	      res = f arr
			  in (n, m, elems arr)
\end{err}
\begin{code}
-- toL :: (Ix i, Num n, NFData n) => Int -> MatArr i n -> TransMat i n
toL :: (Num n, NFData n) => Int -> MatArr Int n -> TransMat Int n
toL shuff arr = let ((_, _), (n, m)) = bounds arr
                    ll = putStrLn "unshuffling matrix..." `pseq`
                         unshuffleN shuff $ elems arr
                    lm = rnf ll `pseq` ll
                    -- in (n, m, lm)
                in ((n, m), lm)
-- fromL :: (Ix i, Num i, Num n) => TransMat i n -> MatArr i n
fromL :: (Num n) => TransMat Int n -> MatArr Int n
-- fromL (n, m, xs) = listArray ((1,1), (n, m)) $ 
fromL ((n, m), xss) = listArray ((1,1), (n, m)) $ 
                      trace "shuffling matrix..." $
                      shuffleN xss

type SparseMatEl i n = ((i, i), n)
type SparseMat i n = ((i, i), [SparseMatEl i n])
type SparseMatShuf i n = ((i, i), [[SparseMatEl i n]])
data SparseMatLift i n = S (SparseMat i n)

instance (NFData i, NFData n) => (NFData (SparseMatLift i n))
instance (Trans i, Trans n) => (Trans (SparseMatLift i n))

unlift :: SparseMatLift i n -> SparseMat i n
unlift (S x) = x

fromS :: Num n => SparseMat Int n -> MatArr Int n
fromS ((n, m), xs) = array ((1,1), (n,m)) xs

fromSm :: Num n => SparseMat Int n -> MatArr Int (Maybe n)
fromSm (b, xs) = let arr = listArray ((1,1), b) $ repeat Nothing
                 in arr // map (\(b, x) -> (b, Just x)) xs

toS :: Num n => MatArr Int n -> SparseMat Int n
toS a = let ((mn,mm), (xn,xm)) = bounds a
            n = xn - mn + 1
            m = xm - mm + 1
        in ((n, m), assocs a) -- fails on partially filled arrays

liftL c f = (toL c) . f . fromL
liftLS f = toS . f . fromL
liftLSd c f = (toSd c) . f . fromL

liftLSl :: (Num n) =>
           (MatArr Int n -> MatArr Int n)
        -> TransMat Int n -> SparseMatLift Int n
liftLSl f = toSl . f . fromL
-- liftB c f = fromL . f . (toL c)
\end{code}

\begin{code}
diagMat :: Num n => MatArr Int n -> MatArr Int (Maybe n)
diagMat a = let b@((mn,_), (xn,_)) = bounds a
                arr = listArray b $ repeat Nothing
            in arr // [((i,i), Just $ a!(i,i)) | i<-[mn..xn] ]
\end{code}

toSd combined toSm and diagMat. We do shuffle of the diagonal with carefully adjusted value.
\begin{code}
diagC :: Int -> Int
diagC = round . sqrt . fromIntegral
-- diagC = id

toSd :: Num n => Int -> MatArr Int n -> SparseMatShuf Int n
toSd c a = let b@((mn,mm), (xn,xm)) = bounds a
               xs = [((i,i), a!(i,i)) | i<-[mn..xn] ]
               xss = unshuffleN (diagC c) xs
           in ((xn-mn+1, xm-mm+1), xss)

fromSd :: Num n => SparseMatShuf Int n -> [n]
fromSd ((n, m), xss) = map snd $ shuffleN xss

toSl :: Num n => MatArr Int n -> SparseMatLift Int n
toSl a = let b@((mn,mm), (xn,xm)) = bounds a
             xs = [((i,i), a!(i,i)) | i<-[mn..xn] ]
         in S ((xn-mn+1, xm-mm+1), xs)

fromSl :: Num n => SparseMatLift Int n -> [n]
fromSl = map snd . snd . unlift
\end{code}

\begin{code}
transposeMap f m = let cs = map f $ transpose $ map elems m
                       rs = listArray (bounds $ head m) cs
                   in rs

-- fromResidue :: (Ix i, Integral n) => [MatArr i (FSingleMod n)] -> MatArr i (Fraction n)
fromResidue = transposeMap (restoreFZ)
-- fromResidueMaybe :: (Ix i, Integral n) => [MatArr i (FSingleMod n)] -> MatArr i (Maybe (Fraction n))
fromResidueMaybe = transposeMap (restoreFZmaybe)
-- fromResidueMaybeDiag :: Integral n => [[FSingleMod n]] -> [Maybe (Fraction n)]
fromResidueMaybeDiag xss = let yss = transpose xss
                               zs = map restoreFZmaybe yss
                           in zs
\end{code}
Homomorphic lift, sequential.

x == lift1p id primes x
\begin{code}
-- lift1p :: (Ix i, Integral n) => 
--           (MatArr i (FSingleMod n) -> MatArr i (FSingleMod n)) 
--        -> [n] 
--        -> MatArr i (Fraction n)
--        -> MatArr i (Fraction n)
lift1p f ps x = let xs = toResiduePrimes x ps
                    ys = fmap f xs
                in fromResidue ys
lift1pm f ps x = let xs = toResiduePrimes x ps
                     ys = fmap f xs
                 in fromResidueMaybe ys
\end{code}

Parallele Version. KAPUTT!
\begin{code}
{-
lift1gpm_inner :: (Ix i, Integral n) => 
                  -- ( (a -> b) -> [a] -> [b] ) -> 
                  ( ( MatArr i (FSingleMod n) -> MatArr i (FSingleMod n) ) ->
                    [MatArr i (FSingleMod n)] -> [MatArr i (FSingleMod n)] )
               -> (MatArr i (FSingleMod n) -> MatArr i (FSingleMod n)) 
               -> [n] 
               -> MatArr i (Fraction n)
               -> MatArr i (Maybe (Fraction n))
-}
lift1gpm_inner mymap f ps x = let xs = toResiduePrimes x ps
                                  ys = mymap f xs
                              in fromResidueMaybe ys
\end{code}
\begin{xcode}
lift1gpm :: (Ix i, Integral n) => 
         ( (a -> b) -> [a] -> [b] ) ->
         -> (MatArr i (FSingleMod n) -> MatArr i (FSingleMod n)) 
         -> [n] 
         -> MatArr i (Fraction n)
         -> MatArr i (Maybe (Fraction n))
-
lift1gpm mymap f ps x = let arrMod = toResiduePrimes x ps
                            listMod = toL arrMod
                            listRes = (mymap (liftL f)) listMod
                            arrRes = fromResidueMaybe listRes
                        in arrRes
\end{xcode}
\begin{code}
{-
lift1gp :: (Ix i, Integral n, Num i) => 
           ((a -> b) -> [a] -> [b])
        -> (MatArr i (FSingleMod n) -> MatArr i (FSingleMod n)) 
        -> [n] 
        -> MatArr i (Fraction n)
        -> MatArr i (Fraction n)
-}
defc = 1000
lift1gp mymap f ps x = let xs' = map (toL defc) $ toResiduePrimes x ps
                           xs = rnf xs' `pseq` 
                                putStrLn "gp: rnf'ed mapped matrix" `pseq` 
                                xs'
                           ys = mymap (liftL defc f) xs
                       in fromResidue $ map fromL ys
{-
#ifdef __EDEN__
-- instance (Trans i, Trans a, Ix i, Num a) => Trans (TransMat i a)
lift1gpm :: (Trans a, Trans n, Num n, Ix i, Integral i, Integral n, Num a) =>
            ((TransMat i a -> TransMat i a) 
                 -> [TransMat i a] -> [TransMat i a])
         -> (MatArr i (FSingleMod n) -> MatArr i (FSingleMod n))
         -> [n]
         -> MatArr i (Fraction n)
         -> MatArr i (Maybe (Fraction n))
#else
lift1gpm :: (Integral n, Ix i, Num i, Num x) =>
            ((a -> b) -> [a] -> [b])
         -> (MatArr i (FSingleMod x) -> MatArr i (FSingleMod x))
         -> [n]
         -> MatArr i (Fraction n)
         -> MatArr i (Maybe (Fraction n))
#endif
-}
lift1gpm mymap f ps x = let xs' = trace "gpm: mapping toL" $
                                  map (toL defc) $ toResiduePrimes x ps
                            xs = rnf xs' `pseq` 
                                 putStrLn "gpm: rnf'ed mapped matrix" `pseq` 
                                 xs'
                            ys = mymap (liftL defc f) xs
                        in trace "gpm: mapping fromL, resulting in a list" $
                           fromResidueMaybe $ map fromL ys

\end{code}

Chunking.
This function returns the whole input
\begin{type}
type Map a b = (a -> b) -> [a] -> [b]
type Worker a b = (a -> b)
type R n = FSingleMod n
instance NFData n => NFData (FSingleMod n)
lift1gpmc :: (Trans n, Num n, Integral n) =>
             Int
          -> Map (TransMat Int (R n)) (TransMat Int (R n))
          -> Worker (MatArr Int (R n)) (MatArr Int (R n))
          -> [n]
          -> MatArr Int (Fraction n)
          -> MatArr Int (Maybe (Fraction n))
\end{type}
\begin{code}
instance (NFData a, Integral a) => NFData (FSingleMod a)
instance (Trans a, NFData a, Integral a) => Trans (FSingleMod a)
instance (NFData a, Integral a) => NFData (Fraction a)
instance (Trans a, NFData a, Integral a) => Trans (Fraction a)

lift1gpmc c mymap f ps x = let xs' = trace "gpmc: mapping toL" $
                                     map (toL c) $ toResiduePrimes x ps
                               xs = rnf xs' 
                                    `pseq` xs'
                               ys = mymap (liftL c f) xs
                           in trace "gpmc: mapping fromL" $
                              fromResidueMaybe $ map fromL ys

\end{code}

Chunking.
This version returns only diagonals!
\begin{xcodex}
-- type TransMat i n = ((i, i), [[n]])
instance NFData x => NFData (FSingleMod x)

-- diagT :: TransMat -> [n]
diagT ((n,m), xss) = let xs = shuffleN xss
                     in [ xs!!(i*n+i) | i<-[0..m-1] ]

lift1gpmcd :: (Trans a, Trans b, Trans i, Trans d,
               Num a, Num b, Num i, Num d,
               NFData a, NFData b, NFData i, NFData d,
               Ix i, Integral i) =>
              Int 
           -> ((a -> a) -> [a] -> [a]) 
           -> (b -> b) 
           -> [Int] 
           -> MatArr i d -> [Maybe d]
lift1gpmcd c mymap f ps x = let xs' = trace "gpmcd: mapping toL" $
                                      (map (toL c) $ toResiduePrimes x ps)
                                -- xs = rnf xs' 
                                --      `pseq` xs'
                                ys = mymap (liftL c f) xs'
                                zs = trace "gpmcd: not mapping back!" $
                                     fromResidueMaybeDiag $ 
                                     shuffleN ys
                            in zs
\end{xcodex}

Transfer representation to children: TransMat
Transfer representation to master: SparseMatShuf of diagonal only
\begin{code}
type Map a b = (a -> b) -> [a] -> [b]
\end{code}
\begin{type}
type M x = MatArr Int x
lift1tss :: (Trans t, Trans s, Trans y, Trans n,
            Num n, Num x, Num y,
            Integral n, Integral x, Integral y) =>
            Int -> Map t s -> (M x -> M x) -> [n] -> M y -> [Maybe y]
\end{type}
\begin{code}
lift1tss c mymap f ps x = let xs = trace "tss: mapping toL" $
                                   (map (toL c) $ toResiduePrimes x ps)
                              ys = mymap (liftLSd c f) xs
                              zs = trace "tss: fromSd and co." $
                                   ( map (restoreFZmaybe) $ 
                                   transpose $
                                   map fromSd ys )
                          in zs

lift1tss2 c mymap f ps x = let xs' = trace "tss: mapping toL with rnf" $
                                     (map (toL c) $ toResiduePrimes x ps)
                               xs = rnf xs' `seq` xs'
                               ys' = trace "tss: mymap with rnf" $
                                     mymap (liftLSd c f) xs
                               ys = rnf ys' `seq` ys'
                               zs = trace "tss: fromSd and co." $
                                    ( map (restoreFZmaybe) $ 
                                      transpose $
                                      map fromSd ys )
                           in zs

-- lift1tsl :: (Trans c, Trans d, Trans e, Integral e) =>
--             Int 
--          -> Map (TransMat Int (FSingleMod e)) (SparseMatLift Int (FSingleMod e))
--          -> (c -> d) -> [e]
--          -> MatArr Int (Fraction e) -> [Maybe (Fraction e)]
lift1tsl c mymap f ps x = let xs = trace "tsl: mapping toL" $
                                   (map (toL c) $ toResiduePrimes x ps)
                              ys = mymap (liftLSl f) xs
                              zs = trace "tsl: fromSl and co." $
                                   ( map (restoreFZmaybe) $ 
                                   transpose $
                                   map fromSl $ ys )
                          in zs

\end{code}
Direct mapping?
\begin{codex}
lift1gpmd mymap f ps x = let xs = map toL $ toResiduePrimes x ps
                             ys = mymap (\i -> liftL f (xs!!i) ) [0..length ps-1] 
                         in fromResidueMaybe $ map fromL ys
\end{codex}

Richtige Skalierung von Primzahlen.
\begin{code}
-- primesScaleMax :: (Integral i) => i -> Integer -> [Integer]
primesScaleMax ma start = let ps = primesFrom start
                              rm = mFromN ma -- compute product of primes from given maximum
                          in scaleHelper ps ([], 1) rm
scaleHelper (p:primes) (ps, acc) ma | acc < ma = scaleHelper primes (p:ps, acc*p) ma
                                    | otherwise = ps
\end{code}