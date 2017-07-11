{-# OPTIONS -fglasgow-exts #-}
-- Eden Project, JB

-- Base module, importing PrimOps => exporting IO actions
-- 
-- version for new Eden module (consistent ChanName' type for createC)
-----------------------------------------------

module Parallel.ParPrim(
     noPe, selfPe     -- system information    :: Int
     , ChanName'      -- primitive channels (abstract in Eden module and outside)
     , fork           -- forking conc. threads :: IO () -> IO ()
     , createC        -- creating placeholders :: IO (ChanName' a, a)
     , connectToPort  -- set thread's receiver :: ChanName' a -> IO ()
     , sendData       -- sending data to recv. :: Mode -> a -> IO ()
     , Mode(..)       -- send modes:  implemented: 
                      --      1 - connect (no graph needed)
                      --      2 - stream  (list element)
                      --      3 - single  (single value)
                      --      4 - rFork   (receiver creates a thread, different ports)
                      -- additional payload (currently only for rFork) in high bits
	      ) 
   where

import GHC.Base(error, Int#, Int(..), (+#), 
		fork#, expectData#,
                connectToPort#, sendData#
	       )
import Control.Parallel.Strategies(NFData(..))
import Control.Parallel.Eden
import Control.DeepSeq
import Control.Monad

----------------------------------------------------------
-- IO wrappers for primitive operations:
--
-- all primitives are implemented out-of-line,
-- wrappers should all be of type * -> IO (...)
--
-- (eden implementation can work with unsafePerformIO)
---------

-- not for export, only abstract type visible outside
data ChanName' a = Chan Int# Int# Int#
                deriving Show

instance NFData a => NFData (ChanName' a) 
    where rnf (Chan pe proc i) = rnf (I# (pe +# proc +# i))

-- tweaking fork primop from concurrent haskell... (not returning threadID)
{-# NOINLINE fork #-}
fork :: IO () -> IO ()
fork action = return (\s -> case (fork# action s) of
                          (# s' , _ #) -> (# s' , () #)
                 )

-- creation of one placeholder and one new inport
{-# NOINLINE createC #-}
-- returns consistent channel type (channel of same type as data)
createC :: IO ( ChanName' a, a )
createC = return (\s -> case (expectData# s) of
                     (# s',i,p, bh #) -> case selfPe of
                                            (# s'', pe #) ->
                                                (# s'',(Chan pe p i, bh) #)
             )

-- TODO: wrap creation of several channels in RTS? (see eden5::createDC# )
--       (would save foreign call overhead, but hard-wire more into RTS)

{-# NOINLINE connectToPort #-}
connectToPort_ :: Int# -> Int# -> Int# -> IO ()
connectToPort_ pe proc i 
    = return (\s -> case (connectToPort# pe proc i s) of
	                   s' -> (# s', () #)
	 )

connectToPort :: ChanName' a -> IO ()
connectToPort (Chan p proc i) = connectToPort_ p proc i

-- send modes for sendData
data Mode = Connect -- announce sender at receiver side (no graph needed)
	  | Data    -- data to send is single value
	  | Stream  -- data to send is element of a list/stream
	  | Instantiate Int -- data is IO(), receiver to create a thread for it
decodeMode :: Mode -> Int
decodeMode Connect         = 1
decodeMode Stream          = 2
decodeMode Data            = 3
decodeMode (Instantiate n) = let k = 4 + n*8
                             in -- k `seq` -- needed to pass NF to PrimOp?
			        k
-- decodeMode other = error "sendData: no such mode"

{-# NOINLINE sendData #-}
sendData :: Mode -> a -> IO ()
sendData mode d 
    = return (\s -> case (sendData# m d s) of
	                   s' -> (# s', () #)
	 )
      where (I# m) = decodeMode mode
