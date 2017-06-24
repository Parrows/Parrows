4-radix FFT, parallel

\begin{code}
{-# OPTIONS -cpp -XParallelListComp #-}
module MathObj.FFT.Radix4Par where

-- import Parallel.Skel.Trivial (dc)

import MathObj.Vector.Partitions
import MathObj.FFT.Complex
-- import qualified MathObj.FFT.Radix2TimeSeq as R2 (fft)
import MathObj.FFT.Short (fftS)
--import qualified Prelude -- hide, hide, hide
import Data.Bool
import Data.Complex
import Data.List(unzip4,zip7)

import qualified MathObj.Vector.Vector as V
import MathObj.Vector.Vector( (@*),(@+),(@-),($*))
--import Prelude ((<), error, (!!), (*), ($), RealFloat)

import MathObj.Vector.Chunking(chunk,unchunk)

import Parallel.Skel.Quad
import Parallel.Eden (noPe, Trans,rnf, NFData)

import Parallel.Skel.DivConN

instance (Trans a, RealFloat a) => Trans (Complex a)
\end{code}

Nussbaumer, s.92
\begin{code}
-- nP = Zahl der PEs, Blattzahl im Baum wird aufgerundet!
fftDo nP cSize xs = dc nP cSize isTrivial fftS (splitR 4) merge xs
    where isTrivial xs = V.length xs < 4
 -- where isTrivial ([]) = True
 --       isTrivial [x] = True
   --     isTrivial [x,y] = True
     --   isTrivial [x,y,z] = True
       -- isTrivial _ = False

-- neue version mit merge2, fest auf noPe 
fftDo2 cSize xs = dc noPe cSize isTrivial fftS (unshuffle' 4) merge2 xs
    where isTrivial xs = V.length xs < 4

-- version mit dcN, fuer test, fest auf noPe 
fftDo3 :: (RealFloat a, Trans a) => Int -> (Int,[Complex a]) -> (Int,[Complex a])
fftDo3 cSize xs = unchunk (
		  dcN' 4 noPe isTrivial fftSC unshuffle'C merge2C 
		     (chunk cSize xs))
    where isTrivial xs = V.length xs < 4
	  -- chunking encoded in worker functions:
	  fftSC = chunk cSize . fftS . unchunk
	  unshuffle'C = map (chunk cSize) . unshuffle' 4 . unchunk
	  merge2C xs = chunk cSize (merge2 (map unchunk xs))

merge,merge2 
      :: (Prelude.Num a,NFData a,RealFloat a) => 
	 [(Prelude.Int,[Complex a])] -> 
	 (Prelude.Int,[Complex a])
\end{code}
merge2 mit allen kommentaren
\begin{nocode}
-- JB: Teillistenerzeugung (Nutzung gleicher Faktoren). 
-- SEHR SCHLECHTE LEISTUNG, unklarer sequenzieller Nachlauf
---------------------------
merge2 [(n0,xs0),(n1,xs1),(n2,xs2),(n,xs3)] 
      | n == n2 && n1 == n0 && n0 == n
	  = rnf temp `seq`
--                           (4*n, r0 ++ r1 ++ r2 ++ r3)
                           ((n,r0) V.++ (n,r1) V.++ (n,r2) V.++ (n,r3))

     where -- n = length xs0
	   w = root_of_unity (-1) (4*n)
	   -- je 4 zwischenwerte gleichzeitig erzeugen:
	   -- (die erste mit "parallel list comprehension" statt zip4)
	   -- einheitswurzeln und teillisten
--	   (_,roots) = powers (4*n) w
-- 	   parts = [(x0,
-- 		     x1*(roots!!k), x2*(roots!!(2*k)), x3*(roots!!(3*k)))
-- 		    | x0 <- xs0
-- 		    | k  <- [0..n-1] --   alle
-- 		    | x1 <- xs1    --    Listen 
-- 		    | x2 <- xs2    -- gleichzeitig
-- 		    | x3 <- xs3 ]  --  durchlaufen
-- 	   (_,xs1',xs2',xs3') = unzip4 parts -- UNUSED!
	   -- Kombinationen 1
--            evensoddsplusminus = [ (x1+x3, x0+x2, (0:+1)*(x1-x3), x0-x2) 
-- 				  | (x0,x1,x2,x3) <- parts ]
--            evensoddsplusminus = [ (x1+x3, x0+x2, (0:+1)*(x1-x3), x0-x2) 
-- 				  | (x0,a1,a2,a3,k) 
-- 				     <- zip5 xs0 xs1 xs2 xs3 [0..n-1]
-- 				  , let x1 = a1*(roots!!k)
-- 				  , let x2 = a2*(roots!!(2*k))
-- 				  , let x3 = a3*(roots!!(3*k))
-- 				]
-- 	   (evensPlus,oddsPlus,twiddle,oddsMinus) = unzip4 evensoddsplusminus
-- 					     -- UNUSED
	   -- Kombinationen 2 (Resultate)
           roots  = snd $ powers (3*n) w   -- take (3*n) (iterate (*w) 1)
           roots2 = snd $ powers n (w*w)   -- take n (iterate (*(w*w)) 1)
           roots3 = snd $ powers n (w*w*w) -- take n (iterate (*(w*w*w)) 1)
	   temp = [ (eP + oP, oM - tW, oP - eP, oM + tW )
--		   | (eP,oP,tW,oM) <- evensoddsplusminus ]
		    | (x0,a1,a2,a3,r1,r2,r3) 
		    <- zip7 xs0 xs1 xs2 xs3 roots roots2 roots3 
--		  , let x1 = a1*(roots!!k)
-- 		  , let x2 = a2*(roots!!(2*k))
-- 		  , let x3 = a3*(roots!!(3*k))
		  , let x1 = a1*r1
          	  , let x2 = a2*r2 -- Version oben: anderes Resultat
		  , let x3 = a3*r3 -- Version oben: anderes Resultat
		  , let (eP,oP,tW,oM) = (x1+x3, x0+x2, (0:+1)*(x1-x3), x0-x2) 
		  ]
	   (r0,r1,r2,r3) = unzip4 temp
merge2 other = error "wrong!"
\end{nocode}
merge2 sauber
\begin{code}
merge2 [(n0,xs0),(n1,xs1),(n2,xs2),(n,xs3)] 
      | n == n2 && n1 == n0 && n0 == n
	  = rnf temp `seq`
            ((n,r0) V.++ (n,r1) V.++ (n,r2) V.++ (n,r3))
      where w = root_of_unity (-1) (4*n)
	    -- je 4 zwischenwerte gleichzeitig erzeugen:
	    -- (die erste mit "parallel list comprehension" statt zip4)
	    -- einheitswurzeln und teillisten
            --
	    -- Kombinationen 2 (Resultate)
            roots  = snd $ powers (3*n) w
            roots2 = snd $ powers n (w*w)
            roots3 = snd $ powers n (w*w*w)
	    temp = [ (eP + oP, oM - tW, oP - eP, oM + tW )
		     | (x0,a1,a2,a3,r1,r2,r3) 
		     <- zip7 xs0 xs1 xs2 xs3 roots roots2 roots3 
		   , let x1 = a1*r1
          	   , let x2 = a2*r2 -- Version oben: anderes Resultat
		   , let x3 = a3*r3 -- Version oben: anderes Resultat
		   , let (eP,oP,tW,oM) = (x1+x3, x0+x2, (0:+1)*(x1-x3), x0-x2) 
		   ]
            (r0,r1,r2,r3) = unzip4 temp
merge2 other = error "wrong!"
\end{code}

Altes merge.
\begin{code}
merge xss = r0 V.++ r1 V.++ r2 V.++ r3
-- merge xss = r0 ++ r3 ++ r2 ++ r1
    where xs0 = xss !! 0
          xs1 = xss !! 1 @* powers n w
          xs2 = xss !! 2 @* powers n (w*w)
          xs3 = xss !! 3 @* powers n (w*w*w)
          n = V.length xs0
          w = root_of_unity (-1) (4*n)
          oddsPlus = xs0 @+ xs2
          evensPlus = xs1 @+ xs3
          oddsMinus = xs0 @- xs2
          twiddle = (xs1 @- xs3) $* (0:+1)
          r0 = oddsPlus @+ evensPlus
          r1 = oddsMinus @- twiddle
          r2 = oddsPlus @- evensPlus
          r3 = oddsMinus @+ twiddle
--          r3 = (oddsMinus @+ twiddle) $* (-1)
\end{code}

Schnittstelle:
\begin{code}
-- nP hier fest auf noPe gesetzt
fft = fftDo noPe

\end{code}
ifft??
