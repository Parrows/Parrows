\begin{code}
module Parallel.SkelHelper where

-- import Parallel.Eden
import Data.List



\end{code}

Another way to create early demand is the core transformation for Eden
processes, switched on with the \verb!-feager!  flag during
compilation.  This transformation applies to all let-blocks in the
program and uses \verb!createProcess!, \verb!deLift! and
\verb!case!-expressions to instantiate a process immediately when its
result is bound directly inside a let expression. The process will be
started, even if the bound variable is not needed at all in the
overall result.

\subsection{Distribution and Combination functions}
These are basic helpers needed for the defined skeletons.
All functionality serves to split and combine lists/streams 
(as process input and output), used in the map skeletons above. 

Each pair of functions (in the order presented here) is mutually 
inverse:\\
\begin{xcode}
(shuffle  . (unshuffleN n)) xs == xs
(shuffleN . (unshuffleN n)) xs == xs
( unchunk . (chunk n))      xs == xs
( unSplit . (splitInto n))  xs == xs
\end{xcode}

Note that \verb!unshuffleN! and \verb!splitIntoN! distribute the input list 
round-robin to \verb!n! sublists, whereas the \verb!chunk! function 
chops off sublists of {\it length} \verb!n! from the beginning of its input.
Thus the usage of these distribution functions is different.

\begin{code}
{- unshuffleN splits a list into n lists
    [takeEach n (drop i xs) | i <- [0..(n-1)]] -}
unshuffleN :: Int -> [a] -> [[a]]
unshuffleN n xs = unshuffle xs
		where  unshuffle xs = map (f xs) [0..n-1]
				where f xs i = g (drop i xs)
				      g [] = []
				      g xs = head xs : (g (drop n xs))

-- simple shuffling (not incremental!)
shuffle :: [[a]] -> [a]
shuffle = concat . transpose

{- 
shuffleN joins n lists which had been splitted with unshuffle
-}
shuffleN :: [[b]] -> [b]
-- shuffleN = concat . transpose 
-- this impl. sequential evaluation on input list after	the other
-- for Eden we need a version, that produces the first outputs as fast
--  as possible, i. e. evaluates all input lists concurrently:
shuffleN xxs 
	| and (map null xxs) = []
	| otherwise = (mymaphead xxs) ++ ( shuffleN (map mytail xxs))
		 where mymaphead [] = []
		       mymaphead ([]:xxs) = mymaphead xxs
		       mymaphead ((x:xs):xxs) = x : mymaphead xxs
		       mytail [] = []
		       mytail xs = tail xs


{- bresenham computes [i1, ..., ip] such that i1 + ... + ip = n
		and | ij - ik | <= 1, for all 1 <= j,k <= n  
    (from computer graphics for printing smooth lines)
-}
bresenham :: Int -> Int -> [Int]
bresenham n p = take p (bresenham1 n)
              where bresenham1 m = (m `div` p) : bresenham1 ((m `mod` p)+ n)


{- Parameterized list splitting: 
splitIntoN distributes one list on n lists with Bresenham distribution
             (equal distribution without precondition on length)
-}
splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n xs = f bh xs
		where bh = bresenham (length xs) n
		      f [] [] = []
		      f [] _  = error "some elements left over"
		      f (t:ts) xs = hs : (f ts rest)
				  where (hs,rest) = splitAt t xs 

unSplit :: [[a]] -> [a]
unSplit = concat

{-
chunk is the simple variant, filling the last list with less elements
             (works best on lists of length k*n)
-}
chunk      :: Int -> [a] -> [[a]]
chunk _ [] =  []
chunk n xs =  ys : chunk n zs
    where (ys,zs) = splitAt n xs

unchunk :: [[a]] -> [a]
unchunk = concat

\end{code}
