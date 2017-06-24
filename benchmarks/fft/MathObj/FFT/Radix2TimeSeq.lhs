Cooly-Tuckey, decimation in time, sequentiell.
\begin{code}
module MathObj.FFT.Radix2TimeSeq where

import Parallel.Skel.Trivial (dc)
import MathObj.Vector.Vector hiding ((++))
import MathObj.Vector.Partitions
import MathObj.FFT.Complex
import qualified Prelude -- hide standard implementations
import Prelude ((==), id, (+), (-), (*), (++), ($), zip, unzip, zip3, error, fromIntegral, take, iterate)
import Data.Complex
\end{code}

Wir verwenden den trivialen DC Skelett.

\begin{code}
fftDo (n, xs) kind = dc isTrivial splitOddEven id (merge kind) (n, xs)
    where isTrivial (n, _) = n==1
\end{code}

Wir  muessen noch merge definieren. Hier ist die parallele Variante.

mix [(l_e,es),(l_o,os)] = -- l_e == l_o !
			  -- 1. teil:
			  rnf temp `seq` -- gleichzeitig l1 und l2 vorberechnen
  				  (2*l_e,liste)			   
    where unitRoots = take l_o (iterate (*root) 1)
	  root      = root_of_unity 1 l_o
	  liste     = l1 ++ l2
	  -- (_,l1) = ((l_e,es)@+((l_o,os)@*(l_o,unitRoots)))
	  -- (_,l2) = ((l_e,es)@-((l_o,os)@*(l_o,unitRoots)))
	  (l1,l2) = unzip temp
	  temp    = [ (e+o',e-o')
		      -- elementweise paare komplett auswerten! besser als ganze liste?
		      -- `using` rnf 
			    | (e,o,r) <- zip3 es os unitRoots, let o' = o*r ]

kind ist hier +/-1 und sorgd fuer die Moeglichkeit ifft zu implementieren.
\begin{nocode}
merge kind [(n, xs), (_, ys)] = (2*n, l1 ++ l2)
    where (l1, l2) = unzip [(e+o', e-o') | (e,o,r) <- zip3 xs ys unitRoots, let o'=o*r]
          unitRoots = toList $ powers n (root_of_unity kind n)
merge _ _ = error "List content mismatch!"
\end{nocode}

\begin{code}
merge _ = mix
mix [(l_e,es),(l_o,os)] = -- l_e == l_o !
			  -- 1. teil:
			  -- rnf temp `seq` -- gleichzeitig l1 und l2 vorberechnen
  				  (2*l_e,liste)			   
    where unitRoots = take l_o (iterate (*root) 1)
	  root      = root_of_unity (1) l_o
	  liste     = l1 ++ l2
          (_,l1) = ((l_e,es)@+ts)
	  (_,l2) = ((l_e,es)@-ts)
          ts = ((l_o,os)@*(l_o,unitRoots))
--	  (l1,l2) = unzip temp
--	  temp    = [ (e+o',e-o')
		      -- elementweise paare komplett auswerten! besser als ganze liste?
		      -- `using` rnf 
--			    | (e,o,r) <- zip3 es os unitRoots, let o' = o*r ]
\end{code}
Nur die zwei Schnittstellen.

\begin{code}
fft (n, xs) = fftDo (n, xs) (1)
-- ifft (n, xs) = (fftDo (n, xs) (-1)) $/ (fromIntegral n:+0)
\end{code}

