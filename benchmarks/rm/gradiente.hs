module Main(main) where
import System
import Parallel.Eden
import IterUntil
import Parallel.Skel.EdenSkel
import System.IO.Unsafe (unsafePerformIO)

pon m x = unsafePerformIO (print m) `seq` x

main
  = do ~[thr,size] <- getArgs
       let size' = read size 
           th = read thr
           m = mA size'
           b = v size'
           np | noPe > th = noPe-1
              | otherwise = noPe
           a = gc np m b
       --pon m (pon b (pon (prMV m a) print a))
       rnf a `seq` putStrLn "done"

v size = take size (drop 27 randomList)
mA size = listToListList size (take (size*size) randomList)
listToListList c m 
 | length m <= c = [m]
 | otherwise = c1 : listToListList c resto
  where (c1,resto) = splitAt c m
  
  
randomList :: [Double]
--randomList = [8,3,2,4]++[1..23]++[19,8]
randomList = map fromInteger (randomList' 30183 357 34713)
randomList' x p m = x: randomList' (mod (x*p) m) p m

------------------------
type Matrix = [Vector]
type Vector = [Double]

-- instance (NFData a,NFData b) => NFData (Either a b) where
--  rnf (Left v)  = rnf v
-- rnf (Right v) = rnf v
instance (Trans a,Trans b) => Trans (Either a b) 

type Task = Either Vector Vector
type SubResult = Either Vector Vector      
type Local = (Matrix,Vector)
type LocalM = (Vector,Vector,Vector,Double,Int)

gc :: Int -> Matrix -> Vector -> Vector
gc np a b = gc' np a b n0s b (map opp b)
 where n0s = replicate (length b) 0
       opp x = -x

gc' np a b x d g = iterUntil (d,g,x,prVV g g,n) abs () split f_it comb
 where as = splitIntoN np a
       bs = splitIntoN np b
--       gs = splitIntoN np g
--       ds = splitIntoN np d
--       xs = splitIntoN np x
       n   = length b
       abs = zip as bs
--       abgdxs = zip6 as bs gs ds xs (repeat n)
      
       split :: () -> [Task] 
       split () = replicate np (Left d)
       
       f_it :: Local -> Task -> (SubResult,Local)
       f_it l t = (f_it' l t,l)
       f_it' (ai,bi) (Right x) = Right newgi
         where newgi = minVV (prMV ai x) bi
       f_it' (ai,bi) (Left d) = Left newad
         where newad = prMV ai d
                              
       
       comb :: LocalM -> [SubResult] -> Either Vector ([Task],LocalM)
       comb (d,g,x,gg,cont) srs@(Left _:_) 
         | cont <= 0 = Left newx
         | otherwise = Right (replicate np (Right newx),(d,g,newx,gg,cont-1))
         where den  = prVV d ad
               ad   = concat (map theLeft srs)
               num  = prVV d g
               s    = -num / den
               newx = addVV x (prEV s d)
         
       comb (d,g,x,gg,cont) srs@(Right _:_) = Right (replicate np (Left newd),(newd,newg,x,num,cont))
         where newg = concat (map theRight srs)
               newd = minVV gds newg
               num  = prVV newg newg
               gdiv = num / gg
               gds  = prEV gdiv d
                  
------------------------      
prVV :: Vector -> Vector -> Double
prVV v1 v2 = sum (zipWith (*) v1 v2)

prMV :: Matrix -> Vector -> Vector
prMV m v = zipWith prVV m (repeat v)

prEV :: Double -> Vector -> Vector
prEV e v = map (e*) v

minVV :: Vector -> Vector -> Vector
minVV = zipWith (-) 

addVV :: Vector -> Vector -> Vector
addVV = zipWith (+)

--zip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = (a,b,c,d,e,f):zip6 as bs cs ds es fs
--zip6 _ _ _ _ _ _ = []

--unzip4 [] = ([],[],[],[])
--unzip4 ((a,b,c,d):xs) = (a:as,b:bs,c:cs,d:ds) 
--  where (as,bs,cs,ds) = unzip4 xs

theLeft  (Left v)  = v
theRight (Right v) = v  

-- t__ = dummySymbols
-- tl__ = createProcessTL pid (1::Int)
{-! global: NFData !-}
