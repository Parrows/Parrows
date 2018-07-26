{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Parrows.CloudHaskell.SimpleLocalNet(
  Thunk(),

  Conf(..),
  State(..),
  defaultInitConf,
  defaultBufSize,

  initializeSlave,
  initializeMaster,

  Evaluatable(..),
  evalTaskBase,

  CloudFuture(..),

  module Data.Binary,
  module Data.Typeable,
  module Control.DeepSeq,
  module Control.Distributed.Process,
  module Control.Distributed.Process.Closure,
  module Control.Distributed.Process.Backend.SimpleLocalnet,
  module Control.Distributed.Process.Node,

  module Parrows.Definition,
  module Parrows.Util,
  module Parrows.CloudHaskell.EvalGen
) where

import Data.Bool

import Data.Array.IO

import Parrows.Definition
import Parrows.Util
import Parrows.Future hiding (get', put')
import Parrows.CloudHaskell.EvalGen

import Control.Arrow

-- packman
import GHC.Packing

import Data.Binary
import Data.Typeable

-- imports for SimpleLocalNet backend compatibility
import Control.Distributed.Process
import Control.Distributed.Process.Node (runProcess, forkProcess, LocalNode, initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

import Control.DeepSeq

import System.IO.Unsafe
import System.Random

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar

import Debug.Trace

data Computation a = Comp {
  computation :: IO (),
  result :: IO a
}

sequenceComp :: [Computation a] -> Computation [a]
sequenceComp comps = Comp { computation = newComp, result = newRes } 
  where newComp = sequence_ $ map computation comps
        newRes = sequence $ map result comps

runComputation :: IO (Computation a) -> a
runComputation x = unsafePerformIO $ do
  comp <- x
  computation comp
  result comp

-- our config type. for now, this only hosts the current state
-- of the backend
type Conf = State

data State = State {
  workers :: MVar [NodeId],
  shutdown :: MVar Bool,
  started :: MVar (),
  localNode :: LocalNode,
  serializeBufferSize :: Int
}

-- | default buffer size used by trySerialize
defaultBufSize :: Int
defaultBufSize = 10 * 2^20 -- 10 MB

defaultInitConf :: LocalNode -> IO Conf
defaultInitConf = initialConf defaultBufSize

initialConf :: Int -> LocalNode -> IO Conf
initialConf serializeBufferSize localNode = do
  workersMVar <- newMVar []
  shutdownMVar <- newMVar False
  startedMVar <- newEmptyMVar
  return State {
    workers = workersMVar,
    shutdown = shutdownMVar,
    started = startedMVar,
    localNode = localNode,
    serializeBufferSize = serializeBufferSize
  }

-- Wrapper for the packman type Serialized
newtype Thunk a = Thunk { fromThunk :: Serialized a } deriving (Typeable)

toThunk a = Thunk { fromThunk = a }

instance (Typeable a) => Binary (Thunk a) where
  put = Data.Binary.put . fromThunk
  get = do
    (ser :: Serialized a) <- Data.Binary.get
    return $ Thunk { fromThunk = ser }

class (Binary a, Typeable a, NFData a) => Evaluatable a where
  evalTask :: (SendPort (SendPort (Thunk a)), SendPort a) -> Closure (Process ())

-- | base evaluation task for easy instance declaration
evalTaskBase :: (Binary a, Typeable a, NFData a) => 
  (SendPort (SendPort (Thunk a)), SendPort a) -> Process ()
evalTaskBase (inputPipe, output) = do
  (sendMaster, rec) <- newChan

  -- send the master the SendPort, that we
  -- want to listen the other end on for the input
  sendChan inputPipe sendMaster

  -- receive the actual input
  thunkA <- receiveChan rec

  -- and deserialize
  a <- liftIO $ deserialize $ fromThunk thunkA

  -- force the input and send it back to master
  sendChan output (rnf a `seq` a)

-- | forces a single value
forceSingle :: (Evaluatable a) => NodeId -> MVar a -> a -> Process ()
forceSingle node out a = do
  -- create the Channel that we use to send the 
  -- Sender of the input from the slave node from
  (inputSenderSender, inputSenderReceiver) <- newChan

  -- create the channel to receive the output from
  (outputSender, outputReceiver) <- newChan

  -- spawn the actual evaluation task on the given node
  -- and pass the two sender objects we created above
  spawn node (evalTask (inputSenderSender, outputSender))

  -- wait for the slave to send the input sender
  inputSender <- receiveChan inputSenderReceiver

  thunkA <- liftIO $ trySerialize a

  -- send the input to the slave
  sendChan inputSender $ toThunk thunkA

  -- TODO: do optional timeout variant so that the computation
  -- runs on the master machine instead
  -- so that we can guarantee results

  -- wait for the result from the slave
  forcedA <- receiveChan outputReceiver

  -- put the output back into the passed MVar
  liftIO $ putMVar out forcedA

-- | evaluates a single value
evalSingle :: Evaluatable a => Conf -> NodeId -> a -> IO (Computation a)
evalSingle conf node a = do
  mvar <- newEmptyMVar
  let computation = forkProcess (localNode conf) $ forceSingle node mvar a
  return $ Comp { computation = computation >> return (), result = takeMVar mvar }

-- | evaluates multiple values 
evalParallel :: Evaluatable a => Conf -> [a] -> IO (Computation [a])
evalParallel conf as = do
  workers <- readMVar $ workers conf

  -- shuffle the list of workers, so we don't end up spawning
  -- all tasks in the same order everytime
  shuffledWorkers <- randomShuffle workers

  -- complete the work assignment node to task (NodeId, a)
  let workAssignment = zipWith (,) (cycle shuffledWorkers) as

  -- build the parallel computation with sequence
  comps <-  sequence $ map (uncurry $ evalSingle conf) workAssignment

  return $ sequenceComp comps

-- | the code for the master node. automatically discovers all slaves and 
-- adds/removes them from the Conf object
master :: Conf -> Backend -> [NodeId] -> Process ()
master conf backend slaves = do
    forever $ do
      shutdown <- liftIO $ readMVar $ shutdown conf
      if shutdown
        then do
          terminateAllSlaves backend
          die "terminated"
        else do
          slaveProcesses <- findSlaves backend
          redirectLogsHere backend slaveProcesses
          let slaveNodes = map processNodeId slaveProcesses
          liftIO $ do
              modifyMVar_ (workers conf) (\_ -> return slaveNodes)
              isEmpty <- isEmptyMVar $ started conf
              if (isEmpty && length slaveNodes > 0) then
                  putMVar (started conf) ()
              else
                return ()

hasSlaveNode :: Conf -> IO Bool
hasSlaveNode conf = isEmptyMVar (started conf)

waitForStartup :: Conf -> IO ()
waitForStartup conf = readMVar (started conf)

newtype CloudFuture a = CF (SendPort (SendPort a))

instance NFData (CloudFuture a) where
  rnf _ = ()

{-# NOINLINE ownLocalConf #-}
ownLocalConf :: Conf
ownLocalConf = unsafePerformIO $ readMVar ownLocalConfMVar

{-# NOINLINE ownLocalConfMVar #-}
ownLocalConfMVar :: MVar Conf
ownLocalConfMVar = unsafePerformIO $ newEmptyMVar

{-# NOINLINE put' #-}
put' :: (Binary a, Typeable a) => Conf -> a -> CloudFuture a
put' conf a = unsafePerformIO $ do
  mvar <- newEmptyMVar
  runProcess (localNode conf) $ do
    (senderSender, senderReceiver) <- newChan

    liftIO $ do
      forkProcess (localNode conf) $ do
        sender <- receiveChan senderReceiver
        sendChan sender a

    liftIO $ putMVar mvar senderSender
  takeMVar mvar >>= (return . CF)

{-# NOINLINE get' #-}
get' :: (Binary a, Typeable a) => Conf -> CloudFuture a -> a
get' conf (CF senderSender) = unsafePerformIO $ do
  mvar <- newEmptyMVar
  runProcess (localNode conf) $ do
    (sender, receiver) <- newChan
    sendChan senderSender sender
    a <- receiveChan receiver
    liftIO $ putMVar mvar a
  takeMVar mvar

instance (ArrowChoice arr, ArrowParallel arr a b Conf) => ArrowLoopParallel arr a b Conf where
    loopParEvalN = parEvalN
    postLoopParEvalN _ = evalN

instance (Binary a, Typeable a) => Future CloudFuture a Conf where
    put = arr . put'
    get = arr . get'

instance (NFData a, Evaluatable b, ArrowChoice arr) => ArrowParallel arr a b Conf where
    parEvalN conf fs = arr (force) >>> evalN fs >>> arr (evalParallel conf) >>> arr runComputation

type Host = String
type Port = String

initializeSlave :: RemoteTable -> Host -> Port -> IO ()
initializeSlave remoteTable host port = do
  backend <- initializeBackend host port remoteTable
  startSlave backend

initializeMaster :: RemoteTable -> Host -> Port -> IO Conf
initializeMaster remoteTable host port = do
	backend <- initializeBackend host port remoteTable

	localNode <- newLocalNode backend

	conf <- defaultInitConf localNode
	putMVar ownLocalConfMVar conf

	-- fork away the master node
	forkIO $ startMaster backend (master conf backend)

	-- wait for startup
	waitForStartup conf
	return conf


-- some utils...

-- | Randomly shuffle a list
--   /O(N)/
-- from: https://wiki.haskell.org/Random_shuffle
randomShuffle :: [a] -> IO [a]
randomShuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs