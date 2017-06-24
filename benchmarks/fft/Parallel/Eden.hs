{-# OPTIONS -cpp  -fglasgow-exts #-}

-- Eden module, defining high-level coordination concepts via
-- Prim.Op.s (which are wrapped inside ParPrim.hs. This version
-- conforms exactly to the code presented at the IFL workshop,
-- regarding functionality, types and function names.

module Parallel.Eden(
	-- reexported from Strategies ---
	NFData(..), using, r0
        ---------- basic Eden -----------
	, noPe, selfPe
	, Process, process, ( # )
	, instantiate
	, instantiateAt     -- explicit placement
	, Trans(..)
	------- dynamic channels --------
	, ChanName          -- Communicator a -> IO(), abstract outside
	, new, parfill      -- using unsafePerformIO
	---------------------------------
	, merge, mergeProc  -- merge, as specified in Eden language, but function!
	---------------------------------
	, Lift(..), deLift, createProcess, cpAt -- deprecated legacy code for Eden 5
    )
  where

#ifndef __PARALLEL_HASKELL__
#warning Compiling  a sequential version of Eden.hs
#endif
    
-- load typecheck simulation if not compiling parallel
#ifndef __SIMUL__
import qualified Parallel.ParPrim as ParPrim
import Parallel.ParPrim hiding(noPe,selfPe)
#else
import qualified Parallel.ParPrimSim as ParPrim
import Parallel.ParPrimSim hiding(noPe,selfPe)
#endif
import Control.Concurrent      -- Instances only
import System.IO.Unsafe(unsafePerformIO)

import Control.Parallel.Strategies -- reexported!
        (NFData(..),using, r0)

--------------------------
-- legacy code for Eden 5:

{-# DEPRECATED deLift, Lift "Lift data type not needed in Eden 6 implementation" #-}
data Lift a = Lift a
deLift :: Lift a -> a
deLift (Lift x) = x

{-# DEPRECATED createProcess "better use instantiate :: Process a b -> a -> IO b instead" #-}
createProcess :: (Trans a, Trans b) 
		 => Process a b -> a -> Lift b
createProcess p i = unsafePerformIO (instantiate p i >>= \x -> return (Lift x))

cpAt :: (Trans a, Trans b) 
		 => Int -> Process a b -> a -> Lift b
cpAt pe p i = unsafePerformIO (instantiateAt pe p i >>= \x -> return (Lift x))


-------------- Eden constructs, also available in seq. version ----------

-- system information
noPe, selfPe :: Int
#if defined(__PARALLEL_HASKELL__)
noPe = unsafePerformIO ParPrim.noPe
selfPe = unsafePerformIO ParPrim.selfPe
#else
noPe = 1
selfPe = 1
#endif

-- processes and instantiation
process       :: (Trans a, Trans b) => (a -> b) -> Process a b
instantiate   :: (Trans a, Trans b) => Process a b -> a -> IO b
instantiateAt :: (Trans a, Trans b) => Int -> Process a b -> a -> IO b
( # )         :: (Trans a, Trans b) => Process a b -> a -> b

#if defined(__PARALLEL_HASKELL__)
data Process a b 
    = Proc (ChanName b ->             -- send back result, overloaded
	    ChanName' (ChanName a) -> -- send input Comm., not overloaded
	    IO ()
	   )
-- '
process f = Proc f_remote
    where f_remote (Comm sendResult) inCC 
	      = do (sendInput, input) <- createComm
		   connectToPort inCC
		   sendData Data sendInput
		   sendResult (f input)

instantiate = instantiateAt 0

instantiateAt p (Proc f_remote) procInput
    = do (sendResult,  r  )     <- createComm  -- result communicator
	 (inCC, Comm sendInput) <- createC     -- reply: input communicator
	 sendData (Instantiate p) 
		  (f_remote sendResult inCC)
	 fork (sendInput procInput)
	 return r

{-# NOINLINE ( # ) #-}
p # x = unsafePerformIO (instantiateAt 0 p x)

#else
-- sequential simulation:
data Process a b = Proc (a -> b)

process f = Proc f
instantiate (Proc f) x 
    = -- rnf fx `seq` -- WRONG: can be tuple with infinite parts
      return fx
    where fx = f x
instantiateAt _ = instantiate
(Proc f) # x = f x
#endif

----------------- merge function, borrowed from Concurrent Haskell -------
merge :: [[a]] -> [a]
merge xss = unsafePerformIO (nmergeIO xss)

mergeProc = merge

---------------------------------------
-- overloading trick: a "communicator" provides a suitable
-- communication function for the overloaded type

-- type Comm a = (a -> IO())
-- JB20061017: leads to obscure runtime errors (suspect: packing code!
-- TODO) when using a simple type alias as described in the
-- paper. Must use an own data type like this:
newtype Comm a = Comm (a -> IO())

-- assumed: contained function sends a over a (previously wired-in) channel

type ChanName a = Comm a -- provide old Eden interface to the outside world

instance NFData (Comm a)

----------------------------
-- Eden-specific operations new/parfill for dynamic channels:

{-# NOINLINE new #-}
new :: Trans a => (ChanName a -> a -> b) -> b
{-# NOINLINE parfill #-}
parfill :: Trans a => ChanName a -> a -> b -> b

#if defined(__PARALLEL_HASKELL__)
parfill (Comm sendVal) val cont 
    = unsafePerformIO (fork (sendVal val) >> return cont)
new chanValCont = unsafePerformIO $ do
	(chan , val) <- createComm
	return (chanValCont chan val)

#else
-- no channel support in seq. version
new _ = error "new: channels not supported"
parfill _ _ v = error "parfill: channels not supported"
#endif

-------------------------------------------------------------------------------------
-- Trans class: overloading communication for streams and tuples

#if defined(__PARALLEL_HASKELL__)
class NFData a => Trans a where
    -- lists/streams written element by element, other types as single
    -- values. All data is evaluated to NF prior to communication
    write :: a -> IO ()
    write x = rnf x `seq` sendData Data x
    -- produce suitable communicator for tuple types:
    createComm :: IO (ChanName a, a)
    createComm = do (cx,x) <- createC
		    return (Comm (sendVia cx) , x)

---------------------------------------
-- Trans Instances:
-------------------
-- (undecidable) default instance, would need GHC flag 
-- -fallow-undecidable-instances. Exact behaviour unclear!
-- Advantage: Defining NFData instance implies Trans with defaults

-- instance NFData a => Trans a

-- "standard types" from Prelude are Transmissible with default
-- communication
instance Trans Int
instance Trans Float
instance Trans Double
instance Trans Char
instance Trans Integer
instance Trans Bool

-- maybe instance using default. Needed often?
instance Trans a  => Trans (Maybe a)

instance Trans () 
-- unit: no communication desired?
-- where
--     write () = error "Eden.lhs: writing unit value"
--     createComm = return (Comm (\_ -> return ()), ())

-- stream communication:
instance (Trans a) => Trans [a]  where 
    write l@[]   = sendData Data l
    write (x:xs) = (rnf x `seq` sendData Stream x) >>
		   write xs

-- "higher-order channels"
instance (NFData a, Trans a) => Trans (ChanName' a)
-- '
instance (NFData a, Trans a) => Trans (Comm a)

-- tuple instances:
instance (Trans a, Trans b) => Trans (a,b)
    where createComm = do (cx,x) <- createC
			  (cy,y) <- createC
			  return (Comm (write2 (cx,cy)),(x,y))
instance (Trans a, Trans b, Trans c) => Trans (a,b,c)
    where createComm = do (cx,x) <- createC
			  (cy,y) <- createC
			  (cz,z) <- createC
			  return (Comm (write3 (cx,cy,cz)),(x,y,z))
instance (Trans a, Trans b, Trans c, Trans d) => Trans (a,b,c,d)
    where createComm = do (ca,a) <- createC
			  (cb,b) <- createC
			  (cc,c) <- createC
			  (cd,d) <- createC
			  return (Comm (write4 (ca,cb,cc,cd)),
				  (a,b,c,d))
instance (Trans a, Trans b, Trans c,  Trans d, Trans e) 
     => Trans (a,b,c,d,e)
    where createComm = do (ca,a) <- createC
			  (cb,b) <- createC
			  (cc,c) <- createC
			  (cd,d) <- createC
			  (ce,e) <- createC
			  return (Comm (write5 (ca,cb,cc,cd,ce)),
				  (a,b,c,d,e))
instance (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f) 
     => Trans (a,b,c,d,e,f)
    where createComm = do (ca,a) <- createC
			  (cb,b) <- createC
			  (cc,c) <- createC
			  (cd,d) <- createC
			  (ce,e) <- createC
			  (cf,f) <- createC
			  return (Comm (write6 (ca,cb,cc,cd,ce,cf)),
				  (a,b,c,d,e,f))
instance (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f, Trans g) 
     => Trans (a,b,c,d,e,f,g)
    where createComm = do (ca,a) <- createC
			  (cb,b) <- createC
			  (cc,c) <- createC
			  (cd,d) <- createC
			  (ce,e) <- createC
			  (cf,f) <- createC
			  (cg,g) <- createC
			  return (Comm (write7 (ca,cb,cc,cd,ce,cf,cg)),
				  (a,b,c,d,e,f,g))
instance (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f, Trans g, Trans h) 
     => Trans (a,b,c,d,e,f,g,h)
    where createComm = do (ca,a) <- createC
			  (cb,b) <- createC
			  (cc,c) <- createC
			  (cd,d) <- createC
			  (ce,e) <- createC
			  (cf,f) <- createC
			  (cg,g) <- createC
			  (ch,h) <- createC
			  return (Comm (write8 (ca,cb,cc,cd,ce,cf,cg,ch)),
				  (a,b,c,d,e,f,g,h))

instance (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f, Trans g, Trans h, Trans i) 
     => Trans (a,b,c,d,e,f,g,h,i)
    where createComm = do (ca,a) <- createC
			  (cb,b) <- createC
			  (cc,c) <- createC
			  (cd,d) <- createC
			  (ce,e) <- createC
			  (cf,f) <- createC
			  (cg,g) <- createC
			  (ch,h) <- createC
			  (ci,i) <- createC
			  return (Comm (write9 (ca,cb,cc,cd,ce,cf,cg,ch,ci)),
				  (a,b,c,d,e,f,g,h,i))
-- bigger tuples use standard communication

------------------------------------------------------------------------------
-- helper functions for Trans class:

-- send function for a single data type (no tuple, non-concurrent)
sendVia :: (NFData a, 
	     Trans a) 
	    => (ChanName' a) -> a -> IO()
sendVia c d = connectToPort c >>
              (sendData Connect d) >> -- optional: connect before evaluation
	      write d

---------------------------------------------------------
-- send functions for tuples...
write2 :: (Trans a, Trans b) => (ChanName' a, ChanName' b) -> (a,b) -> IO ()
write2 (c1,c2) (x1,x2) = do 
        fork (sendVia c1 x1)
        sendVia c2 x2
write3 :: (Trans a, Trans b, Trans c) 
        => (ChanName' a, ChanName' b, ChanName' c) -> (a,b,c) -> IO ()
write3 (c1,c2,c3) (x1,x2,x3) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        sendVia c3 x3
write4 :: (Trans a, Trans b, Trans c, Trans d) 
        => (ChanName' a, ChanName' b, ChanName' c, ChanName' d
	   ) -> (a,b,c,d) -> IO ()
write4 (c1,c2,c3,c4) (x1,x2,x3,x4) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        fork (sendVia c3 x3)
	sendVia c4 x4
write5 :: (Trans a, Trans b, Trans c, Trans d, Trans e) 
        => (ChanName' a, ChanName' b, ChanName' c, ChanName' d, ChanName' e
	   ) -> (a,b,c,d,e) -> IO ()
write5 (c1,c2,c3,c4,c5) (x1,x2,x3,x4,x5) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        fork (sendVia c3 x3)
        fork (sendVia c4 x4)
        sendVia c5 x5
write6 :: (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f) 
        => (ChanName' a, ChanName' b, ChanName' c, ChanName' d, 
	    ChanName' e, ChanName' f
	   ) -> (a,b,c,d,e,f) -> IO ()
write6 (c1,c2,c3,c4,c5,c6) (x1,x2,x3,x4,x5,x6) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        fork (sendVia c3 x3)
        fork (sendVia c4 x4)
        fork (sendVia c5 x5)
        sendVia c6 x6
write7 :: (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f, Trans g) 
        => (ChanName' a, ChanName' b, ChanName' c, ChanName' d, 
	    ChanName' e, ChanName' f, ChanName' g
	   ) -> (a,b,c,d,e,f,g) -> IO ()
write7 (c1,c2,c3,c4,c5,c6,c7) (x1,x2,x3,x4,x5,x6,x7) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        fork (sendVia c3 x3)
        fork (sendVia c4 x4)
        fork (sendVia c5 x5)
        fork (sendVia c6 x6)
        sendVia c7 x7
write8 :: (Trans a, Trans b, Trans c, Trans d, Trans e, Trans f, Trans g, Trans h) 
        => (ChanName' a, ChanName' b, ChanName' c, ChanName' d, 
	    ChanName' e, ChanName' f, ChanName' g, ChanName' h 
	   ) -> (a,b,c,d,e,f,g,h) -> IO ()
write8 (c1,c2,c3,c4,c5,c6,c7,c8) (x1,x2,x3,x4,x5,x6,x7,x8) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        fork (sendVia c3 x3)
        fork (sendVia c4 x4)
        fork (sendVia c5 x5)
        fork (sendVia c6 x6)
        fork (sendVia c7 x7)
        sendVia c8 x8
write9 :: (Trans a,Trans b,Trans c,Trans d,Trans e,Trans f,Trans g,Trans h,Trans i) 
        => (ChanName' a, ChanName' b, ChanName' c, ChanName' d, 
	    ChanName' e, ChanName' f, ChanName' g, ChanName' h, ChanName' i
	   ) -> (a,b,c,d,e,f,g,h,i) -> IO ()
write9 (c1,c2,c3,c4,c5,c6,c7,c8,c9) (x1,x2,x3,x4,x5,x6,x7,x8,x9) = do 
        fork (sendVia c1 x1)
        fork (sendVia c2 x2)
        fork (sendVia c3 x3)
        fork (sendVia c4 x4)
        fork (sendVia c5 x5)
        fork (sendVia c6 x6)
        fork (sendVia c7 x7)
        fork (sendVia c8 x8)
        sendVia c9 x9

#else
class NFData a => Trans a -- where nothing happens
    where dummyMethod :: a -> a -- dummy method avoids GHC warning
	  dummyMethod _ = error "dummyMethod"

instance (Trans a) => Trans [a] 
instance Trans ()
instance Trans Int
instance Trans Float
instance Trans Double
instance Trans Char
instance Trans Integer
instance Trans Bool

-- maybe instance using default. Needed often?
instance Trans a  => Trans (Maybe a)

-- Trans instances for tuples
instance (Trans x, Trans y) 
    => Trans (x,y) 
instance (Trans a, Trans b, Trans c) 
    => Trans (a,b,c) 
instance (Trans a, Trans b, Trans c, Trans d) 
    => Trans (a,b,c,d) 
instance (Trans a, Trans b, Trans c,  
	  Trans d, Trans e) => Trans (a,b,c,d,e) 
instance (Trans a, Trans b, Trans c,  
	  Trans d, Trans e,Trans f) => Trans (a,b,c,d,e,f) 
instance (Trans a, Trans b, Trans c,  Trans d, 
	  Trans e,Trans f,Trans g) => Trans (a,b,c,d,e,f,g) 
instance (Trans a, Trans b, Trans c,  Trans d, 
	  Trans e,Trans f,Trans g,Trans h) 
    => Trans (a,b,c,d,e,f,g,h)
instance (Trans a, Trans b, Trans c,  Trans d, 
	  Trans e,Trans f,Trans g,Trans h, Trans i) 
    => Trans (a,b,c,d,e,f,g,h,i)
#endif
