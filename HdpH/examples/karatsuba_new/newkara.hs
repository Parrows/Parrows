{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -cpp #-}
module Main where
-- import Eden
import System.Environment
-- import DivConN
import DivConRW hiding (rnf)

import Control.DeepSeq

import Control.Monad
import Data.Serialize

import System.Environment (getArgs)
import System.IO (stdout, stderr, hSetBuffering, BufferMode(..))


import Control.Parallel.HdpH.Closure
import Control.Parallel.HdpH hiding (put, get)
import Control.Parallel.HdpH.Strategies

import Prelude hiding (seq) -- exported in DC
--import Control.Parallel.Eden (Trans)

-- parse runtime system config options; abort if there is an error
parseOpts :: [String] -> IO (RTSConf, [String])
parseOpts args = do
  either_conf <- updateConf args defaultRTSConf
  case either_conf of
    Left err_msg                 -> error $ "parseOpts: " ++ err_msg
    Right (conf, remaining_args) -> return (conf, remaining_args)



usage = "I need 3 parameters!, parallel depth, "
    ++ "no. of digits (for number 1 and 2)"

main = let
       i1' = read "8000"
       i2' = read "8000"
       d'  = read "4"
       is  = concat (repeat [1..9])
       mi1 = take i1' is
       mi2 = take i2' is
       tmp conf = karat conf 2 mi1 mi2
       tmpSeq conf = karat conf (-1) mi1 mi2
       in do 
          hSetBuffering stdout LineBuffering
  	  hSetBuffering stderr LineBuffering
  	  opts_args <- getArgs
          print opts_args
          (conf, args) <- parseOpts opts_args 
          print $ length (fromD (tmp conf d'))

           --print $ length (fromD (tmp d'))
           --defaultMain [ bgroup ((show i1') ++ ", " ++ (show i2') ++ ", " ++ (show d') ++ " depth") [
           --                                 bench "par" $ nf tmp d'
           --                            ]
           --             ]
		   --print $ ((show i1') ++ ", " ++ (show i2') ++ ", " ++ (show d') ++ " depth")

{-main = do
           args <- getArgs
           if length args < 3
           then putStrLn usage
           else
                let
                    i1' = read i1
                    i2' = read i2
                    d'  = read d
                    is  = concat (repeat [1..9])
                    mi1 = take i1' is
                    mi2 = take i2' is
                    (d:i1:i2:_) = args
                    tmp = karat 2 mi1 mi2
                in
                defaultMain [
                    bgroup ("karat " ++ d ++ ", " ++ i1 ++ ", " ++ i2) [ bench "par" $ whnf tmp d',
                    bench "seq" $ whnf tmp 0
                ]]
-}


fromMyInteger :: MyInteger -> Integer
fromMyInteger [] = 0
fromMyInteger (i:is) = (fromIntegral i) + (fromIntegral base)*(fromMyInteger is)
           
           
--main = print (karatSeq ([1..8], [5..9]++[0..4]))


#if !(defined STREAM)

-- HAAACK!
data D a = D a deriving Show
instance NFData a => NFData (D a) where
    rnf (D a) = rnf a
instance Serialize a => Serialize (D a) where
    put (D a) = put a
    get       = liftM D get

instance ToClosure [Int] where locToClosure = $(here)
instance ToClosure [D MyInteger] where locToClosure = $(here)
instance ToClosure a => ToClosure (D a) where locToClosure = $(here)
    
instance ForceCC MyInteger where locForceCC = $(here)
instance ForceCC [D MyInteger] where locForceCC = $(here)
instance ForceCC a => ForceCC (D a) where locForceCC = $(here)
    
--instance Trans a => Trans (D a)

fromD (D x) = x

#else
#define D 
fromD x = x
#endif

type MyInteger = [Int]
type MyIntegerS = (MyInteger,Bool) -- False <==> negative
base::Int
base = 10   -- To do: Meter dentro del tipo?      


karat :: RTSConf -> Int -> MyInteger -> MyInteger -> Int -> D MyInteger
-- karat 0 depth is1 is2 = dcN_c 3 depth trivial solve split combine (D (is1,is2))
-- karat 1 depth is1 is2 = dcNTickets_c 3 tickets trivial solve split combine seqDC 
--			             (D (is1,is2))
--  where tickets = [2..noPe]
--	seqDC x = if trivial x then solve x else combine x (map seqDC (split x))
karat  conf 2 is1 is2 depth = divConRW conf 2 depth trivial solve split combine (D (is1,is2))
karat conf (-1) is1 is2 depth = divConRW conf 0 depth trivial solve split combine (D (is1,is2))
-- karat 3 depth is1 is2 = divConFarm depth trivial solve split combine (D (is1,is2))
--

karat conf _ _ _ _ = D [] -- error...

trivial  (D(is1,is2)) = lmin <= 10
 where lmin = min (length is1) (length is2)
split (D (is1,is2)) = 
                      -- reverse -- first task easier than rest?
                      [D(is1a,is2a),D(is1b,is2b),D(sumSeq is1a is1b,sumSeq is2a is2b)]
 where l1 = length is1
       l2 = length is2
       lmax = max l1 l2
       ldiv = lmax `div` 2 
       (is1b,is1a) = splitAt ldiv is1
       (is2b,is2a) = splitAt ldiv is2
combine (D(is1,is2)) [D u,D v,D w] = D result
                     -- [D w,D v,D u] = D result -- reverse above
 where l1 = length is1
       l2 = length is2
       lmax = max l1 l2
       ldiv = lmax `div` 2  
       u0s = replicate (2*ldiv) 0 ++ u
       v0s = v
       (wuv,wuvS) = resSeqS (w,True) ((sumSeq u v),True)
       wuv0s = (replicate ldiv 0 ++ wuv,wuvS)
       result = fst (sumSeqS (sumSeq u0s v0s,True) wuv0s)     
solve (D(is1,is2)) = D(multSeq' isSmall isBig)
 where l1 = length is1
       l2 = length is2
       (isSmall,isBig) = if l1 > l2 then (is2,is1) else (is1,is2)     


       
       
multSeq' isSmall isBig = foldl{-'-} sumSeq [] iss0s
 where iss = [multEsc i isBig | i <- isSmall]
       iss0s = zipWith (++) [replicate n 0| n <- [0..]]  iss
                     
multEsc :: Int -> MyInteger -> MyInteger
multEsc i is = normalize 0 (map (i*) is)

normalize i is = noCeros (normalize' i is)
noCeros is = reverse (dropWhile (==0) (reverse is))
normalize' 0 [] = []
normalize' i [] = [i]
normalize' i (i1:is) = isum:normalize' iacc is
 where itot = i + i1
       isum = itot `mod` base
       iacc = itot `div` base
           
                 

-- Adding two arbitrary precision integers
sumSeq :: MyInteger -> MyInteger -> MyInteger
sumSeq is1 is2 = normalize 0 (zipWith (+) is1' is2')
 where l1 = length is1
       l2 = length is2
       (is1',is2') = if l1 > l2 then (is1,is2++replicate (l1-l2) 0) 
                                else (is1++replicate (l2-l1) 0,is2)
           
resSeq :: MyInteger -> MyInteger -> MyInteger
resSeq is1 is2 = normalize 0 (zipWith (-) is1 (is2++repeat 0))

sumSeqS (is1,b1) (is2,b2)
 | b1==b2 = (sumSeq is1 is2,b1)
 | mayor is1 is2 = (resSeq is1 is2,b1)
 | otherwise = (resSeq is2 is1,b2)
 where mayor a b = True  -- To do: Cambiar representaci�n para no 
                         --        necesitar reverse en las comparaciones?

resSeqS is1S (is2,b2) = sumSeqS is1S (is2,not b2)
