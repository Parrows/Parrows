{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
-- Eden Project, JB

-- Base module, importing PrimOps => exporting IO actions
-- 
-- version for new Eden module (consistent ChanName' type for createC)
-----------------------------------------------

module ParPrimSim
{-
    (noPe, selfPe     -- system information    :: Int
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
-- -}
   where

#ifdef __PARALLEL_HASKELL__
#error Both Parallel Haskell and Simulation requested
#endif
import GHC.Base -- primitive types

import Data.Dynamic

import System.IO.Unsafe
import Control.Concurrent
import Control.Parallel.Strategies(NFData(..))

----------------------------------------------------------
-- Concurrent-Haskell simulation of Eden PrimOps
--

-- DEBUG HACKS
testChan :: Typeable a => ChanName' a
testChan = Chan 1# 2# 3#


---- Simulation specials (CAF trick) ----

{-# NOINLINE idSupply #-}
idSupply :: MVar Int
idSupply = unsafePerformIO (newMVar 1)

freshId :: IO Int
freshId = do 
            i <- takeMVar idSupply
            putMVar idSupply (i+1)
            return i

{-# NOINLINE thrs #-}
thrs :: MVar [(ThreadId, Int, Maybe Int)]
thrs = unsafePerformIO 
         (myThreadId >>= \id -> 
          newMVar [(id,1,Nothing)])

myProc :: IO Int
myProc = do id <- myThreadId 
            ths <- readMVar thrs
            return (fst $ findProc id ths)
myChan :: IO Int
myChan = do id <- myThreadId
            ths <- readMVar thrs
            let cid = snd $ findProc id ths
            case cid of 
              Nothing -> error $ show id ++ " not connected"
              Just c  -> return c

findProc :: ThreadId -> [(ThreadId,Int,b)] -> (Int,b)
findProc id [] = error $ "process not found for id " ++ show id
findProc id ((tid,p,c):rest) | id == tid = (p,c)
                             | otherwise = findProc id rest

removeThread :: ThreadId -> IO ()
removeThread id = do tlist <- takeMVar thrs
                     let newList = rmv tlist
                     putMVar thrs newList
    where rmv [] = []
          rmv (x@(tid,_,_):rest) | id == tid = rest
                                 | otherwise = x:rmv rest

{-# NOINLINE chs #-}
chs :: MVar [(Int,Dynamic)]
chs = unsafePerformIO (newMVar [])

varC :: Typeable a => Int -> IO (MVar a)
varC id = do clist <- readMVar chs
             case lookup id clist of
                     Nothing -> error $ "missing MVar for Id " 
                                        ++ show id
                     Just varD -> return (fromDyn varD err)

--------- Primitives simulation ----------

-- system information
{-# NOINLINE noPe #-}
noPe :: IO Int
noPe = return 4

{-# NOINLINE selfPe #-}
selfPe :: IO Int
selfPe = do pe <- myProc
            return (1 + pe `mod` 4)

-- not for export, only abstract type visible outside
data ChanName' a = Chan Int# Int# Int#
                deriving (Show,Typeable)

instance NFData a => NFData (ChanName' a) 
    where rnf (Chan pe proc i) = rnf (I# (pe +# proc +# i))

-- tweaking fork primop from concurrent haskell... (not returning threadID)
{-# NOINLINE fork #-}
fork :: IO () -> IO ()
fork action = do p <- myProc
                 tList <- takeMVar thrs
                 id <- forkIO action'
                 putMVar thrs ((id,p,Nothing):tList)
    where action' = do action
                       id <- myThreadId
                       removeThread id 

-- creation of one placeholder and one new inport
{-# NOINLINE createC #-}
-- returns consistent channel type (channel of same type as data)
createC :: Typeable a => IO ( ChanName' a, a )
createC = do var <- newEmptyMVar
             (I# id) <- freshId
             (I# pe) <- selfPe
             (I# p)  <- myProc 
             cList <- takeMVar chs
             let c = Chan pe p id
                 x = unsafePerformIO (readMVar var)
                 dv = toDyn var
             putMVar chs (((I# id), dv):cList)
             return (c,x)

-- connect a thread to a channel
connectToPort :: ChanName' a -> IO ()
connectToPort (Chan _ _ cid)
                   = do id <- myThreadId
                        tlist <- takeMVar thrs
                        putMVar thrs (replace id tlist)
      where replace id [] = error $ "not found: id " ++ show id
            replace id (x@(tid,p,_):rest) 
                | id == tid = (tid,p,Just (I# cid)):rest
                | otherwise = x:(replace id rest)

-- send modes for sendData
data Mode = Connect -- announce sender at receiver side (no graph needed)
	  | Data    -- data to send is single value
	  | Stream  -- data to send is element of a list/stream
	  | Instantiate Int -- data is IO(), receiver to create a thread for it

{-# NOINLINE sendData #-}
sendData :: Typeable a => Mode -> a -> IO ()
sendData Connect _ = return ()
sendData (Instantiate pe) d 
         = do p <- myProc
              tlist <- takeMVar thrs
              id <- forkIO action -- (unsafeCoerce action)
              putMVar thrs ((id,p+1,Nothing):tlist)
    where action = do toIO d
                      id <- myThreadId
                      removeThread id 

sendData Data d = do id <- myThreadId
                     cd <- myChan
                     var <- varC cd
                     putMVar var d
sendData Stream d = do id <- myThreadId
                       cd <- myChan
                       var <- varC cd
                       v2 <- newEmptyMVar
                       let x = unsafePerformIO $
                                  readMVar v2
                       putMVar var (d:x)

err = error "simulation error in dynamically typed value"

toIO :: Typeable a => a -> IO ()
toIO x = case cast x of
           Nothing -> error "wrong cast"
           Just io -> io
