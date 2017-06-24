{-# OPTIONS -cpp #-}
module Main where

-- import MathObj.FFT.Radix4Par as R4 (fft,fftDo2,fftDo3)
import MathObj.FFT.Radix4Seq as R4 (fft)
import MathObj.FFT.Reference as R2 (fft, ifft)
import MathObj.FFT.MultiPar (twoDimFft, twoDimFftG, twoDimFftRD)
import Prelude
-- import System.CPUTime
import Data.Complex
import MathObj.FFT.Complex
import Control.Concurrent
#ifdef TEST
import qualified MathObj.FFT.Reference as Ref
#endif

import System.Environment (getArgs)
import System.IO
import Control.Parallel.Eden (rnf, Trans)
import Control.Parallel.Eden.Map (farmS)

instance (RealFloat a, Trans a) => Trans (Complex a) -- ???

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "usage: testfftTest <size or base for power of 4(1024) of one side> <version 0,1,2,3,4(3)> <chunksize(512), no effect?> <prefetch(20)>"
  args <- getArgs
  let f = mkLists n
--      n = if null args then 512 else read (head args)
      n = if b > 15 then b else 4^b
      b = if null args then 1024 else read (head args)
      v = if length args < 2 then 3 else read (args!!1)
      s = if length args < 3 then 512 else read (args!!2)
      p = if length args < 4 then 20 else read (args!!3)
--  let r = fftDo3 s (n, f)
--      r2 = fftDo2 s (n,f)
      r4 = twoDimFft n n f
      r5 = twoDimFftG map R4.fft n n f
      r6 = twoDimFftG (farmS p) R2.fft n n f
      r7 = twoDimFftG (farmS p) R4.fft n n f
      r8 = twoDimFftRD (farmS p) R4.fft n n f
#ifdef TEST
  let (len,resultlist) = r
  putStrLn ("Length given as " ++ show len ++ 
	    ", in reality " ++ show (length resultlist))
  let ref = Ref.fft (n,f)
  if (r /= ref) then putStrLn "WRONG RESULT" else putStrLn "CORRECT"
  rnf r2 `seq` 
--  (length (snd r)) `seq`
--  (fst r ) `seq` 
    putStrLn "Done version fftDo2"
  threadDelay 100000
  rnf r `seq` 
--  (length (snd r)) `seq`
--  (fst r ) `seq` 
    putStrLn "Done version fft"
  if r == r2 
     then putStrLn "same result" 
     else outputDiff 5 r r2
#else
  case v of 
--     0 -> rnf r `seq` putStrLn ("Done, version fft")
--     1 -> rnf r2 `seq` putStrLn ("Done, version fftDo2")
     2 -> putStrLn ("not defined in this version, look in code/small/")
     3 -> rnf r4 `seq` putStrLn ("Done, version twoDimFft with seqmap R2 (sequential ground truth for R2)")
     4 -> rnf r5 `seq` putStrLn ("Done, version twoDimFft with seqmap R4 (sequential ground truth for R4)")
     5 -> rnf r6 `seq` putStrLn ("Done, version twoDimFft with farm R2")
     6 -> rnf r7 `seq` putStrLn ("Done, version twoDimFft with farm R4")
     7 ->rnf r7 `seq` putStrLn ("Done, version twoDimFftRD(!) with farm R4")
     _ -> putStrLn ("no version " ++ show v ++ " defined")
#endif

mkLists :: Int -> [[Complex Double]]
mkLists n = map (\k -> lift $ [k..k+n]) [1..n]

outputDiff :: (Show a,Eq a,Num a) => Int -> (Int,[a]) -> (Int,[a]) -> IO ()
outputDiff n (n1,l1) (n2,l2) 
    | n1 /= n2  = putStrLn "different length"
    | otherwise = do putStrLn (show (length diffs) ++ " different elements")
		     putStrLn (unlines (map show (take n diffs)))
    where diffs = [ (k,x,y,x-y) | (k,x,y) <- zip3 [0..] l1 l2, x/=y ]
