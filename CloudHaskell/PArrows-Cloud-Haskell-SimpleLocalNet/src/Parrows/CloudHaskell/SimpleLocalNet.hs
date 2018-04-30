{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Parrows.CloudHaskell.SimpleLocalNet(
  Thunk(),

  Conf(..),
  State(..),
  defaultConf,
  defaultBufSize,

  BackendType(..),
  startBackend,

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

import GHC.Magic

import Data.Binary
import Data.Typeable

-- imports for SimpleLocalNet backend compatibility
import Control.Distributed.Process
import Control.Distributed.Process.Node (runProcess, forkProcess, LocalNode(..), initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

import Control.DeepSeq

import System.IO.Unsafe
import System.Random

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent(forkIO, threadDelay, yield)
import Control.Concurrent.MVar

import Debug.Trace

data Computation a = Comp {
  computation :: IO (),
  result :: MVar a
}

sequenceComp :: (NFData a) => [Computation a] -> IO (Computation [a])
sequenceComp comps = do
  mvar <- newEmptyMVar
  return Comp { 
    computation = newComp comps mvar,
    result = mvar
  } 
    where 
      newComp :: [Computation a] -> MVar [a] -> IO ()
      newComp comps mvar = do
            -- run the computations
            sequence_ $ map computation comps

            putStrLn "preSequence"
            let resMVars = map result comps
            putStrLn $ "res MVars: " ++ (show $ length resMVars)

            res <- resMVars `seq` sequence $ map (takeMVar) resMVars
            putStrLn "postSequence"
            putMVar mvar res

-- our internal Par Monad, we just use IO for now
type Par a = IO (Computation a)

runPar :: Par a -> a
runPar x = unsafePerformIO $ do
  comp <- x
  computation comp
  takeMVar $ result comp

-- our config type. for now, this only hosts the current state
-- of the backend
data Conf = Conf {
  workers :: [NodeId],
  serializeBufferSize :: Int
}

data State = State {
  workersMVar :: MVar [NodeId],
  shutdown :: MVar Bool,
  started :: MVar Bool,
  localNode :: LocalNode
}

-- | default buffer size used by trySerialize
defaultBufSize :: Int
defaultBufSize = 100 * 2^20 -- 50 MB

defaultConf :: IO Conf
defaultConf = do
  workers <- readMVar $ workersMVar $ localState
  return Conf {
    workers = workers,
    serializeBufferSize = defaultBufSize
  }

initialState :: LocalNode -> IO State
initialState localNode = do
  workersMVar <- newMVar []
  shutdownMVar <- newMVar False
  startedMVar <- newMVar False
  return State {
    workersMVar = workersMVar,
    shutdown = shutdownMVar,
    started = startedMVar,
    localNode = localNode
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
forceSingle :: (Evaluatable a) => Conf -> NodeId -> MVar a -> a -> Process ()
forceSingle conf node out a = do
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

  debug $ "preSerialize " ++ (show $ typeOf $ a)

  thunkA <- liftIO $ trySerializeWith a (serializeBufferSize conf)

  debug "postSerialize"

  -- send the input to the slave
  sendChan inputSender $ toThunk thunkA

  -- TODO: do optional timeout variant so that the computation
  -- runs on the master machine instead
  -- so that we can guarantee results

  -- wait for the result from the slave
  forcedA <- receiveChan outputReceiver

  -- put the output back into the passed MVar
  liftIO $ putMVar out forcedA

-- | evaluates a single value inside the Par monad
evalSingle :: Evaluatable a => Conf -> NodeId -> a -> Par a
evalSingle conf node a = do
  mvar <- newEmptyMVar 
  return $ Comp { 
      computation = do
        pid <- forkProcess (localNode localState) $ forceSingle conf node mvar a
        putStrLn $ "pid" ++ show pid
      ,result = mvar
  }

-- | evaluates multiple values inside the Par monad
evalParallel :: Evaluatable a => Conf -> [a] -> Par [a]
evalParallel conf as = do
  -- shuffle the list of workers, so we don't end up spawning
  -- all tasks in the same order everytime
  shuffledWorkers <- randomShuffle $ workers conf

  putStrLn $ show shuffledWorkers

  -- complete the work assignment node to task (NodeId, a)
  let workAssignment = zipWith (,) (cycle $ shuffledWorkers) as

  -- build the parallel computation with sequence
  comps <- sequence $ map (uncurry $ evalSingle conf) workAssignment

  res <- sequenceComp comps
  return res

-- | the code for the master node. automatically discovers all slaves and 
-- adds/removes them from the Conf object
master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    forever $ do
      shutdown <- liftIO $ readMVar $ shutdown localState
      if shutdown
        then do
          terminateAllSlaves backend
          die "terminated"
        else do
          slaveProcesses <- findSlaves backend
          let slaveNodes = map processNodeId slaveProcesses
          liftIO $ do
              modifyMVar_ (workersMVar localState) (\_ -> return slaveNodes)
              if (length slaveNodes) > 0 then
                modifyMVar_ (started localState) (\_ -> return True)
              else
                return ()

waitUntil condition = fix $ \loop -> do
  cond <- condition
  if cond
    then return ()
    else threadDelay 100 >> loop

hasSlaveNode :: State -> IO Bool
hasSlaveNode state = readMVar (started state)

-- | wait for the (started Conf) == true (i.e. the master node has found slaves)
waitForStartup :: State -> IO ()
waitForStartup state = waitUntil (hasSlaveNode state)

newtype CloudFuture a = CF (SendPort (SendPort a))

instance (Typeable a, Binary a) => Binary (CloudFuture a) where
    put (CF sp) = Data.Binary.put sp
    get = do
        val <- Data.Binary.get
        return $ CF val

instance (NFData a) => NFData (CloudFuture a) where
  rnf (CF sp) = rnf sp

{-# NOINLINE localState #-}
localState :: State
localState = unsafePerformIO $ do
  locState <- readMVar localStateMVar
  putStrLn $ "workers from conf: " ++ show (unsafePerformIO $ readMVar $ workersMVar locState)
  return locState

{-# NOINLINE localStateMVar #-}
localStateMVar :: MVar State
localStateMVar = unsafePerformIO $ newEmptyMVar

isDebug :: Bool
isDebug = True

debug :: (Show a) => a -> Process ()
debug a = if isDebug then liftIO $ putStrLn $ show a else return ()

{-# NOINLINE put' #-}
put' :: (NFData a, Binary a, Typeable a) => Conf -> a -> CloudFuture a
put' conf a = unsafePerformIO $ do
  mvar <- newEmptyMVar
  forkProcess (localNode localState) $ do
    (senderSender, senderReceiver) <- newChan
    debug $ (typeOf senderSender)
    liftIO $ putMVar mvar senderSender
    debug $ "put mvar"
    debug $ "pre receive sender"
    debug $ "type: " ++ (show $ typeOf $ senderReceiver)
    sender <- receiveChan senderReceiver
    debug $ "received sender"
    sendChan sender (rnf a `seq` a)
    debug $ "sent"
  takeMVar mvar >>= (return . CF)

{-# NOINLINE get' #-}
get' :: (NFData a, Binary a, Typeable a) => Conf -> CloudFuture a -> a
get' conf (CF senderSender) = unsafePerformIO $ do
  mvar <- newEmptyMVar
  forkProcess (localNode localState) $ do
    debug $ "toast"
    (sender, receiver) <- newChan
    debug $ "created sender: " ++ (show $ sender)
    sendChan senderSender sender
    debug $ "sent sender over senderSender: " ++ (show $ senderSender) ++ " " ++ (show $ sender)
    debug $ "get' : pre receiveChan receiver"
    debug $ "type: " ++ (show $ typeOf $ receiver)
    a <- receiveChan receiver
    debug $ "kartoffel"
    liftIO $ putMVar mvar a
  print "before takeMVar"
  val <- takeMVar mvar
  return val

instance (ArrowChoice arr, ArrowParallel arr a b Conf) => ArrowLoopParallel arr a b Conf where
    loopParEvalN _ = evalN
    postLoopParEvalN = parEvalN

instance (NFData a, Binary a, Typeable a) => Future CloudFuture a Conf where
    put = arr . put'
    get = arr . get'

instance (NFData a, Evaluatable b, ArrowChoice arr) => ArrowParallel arr a b Conf where
    parEvalN conf fs = arr force >>> evalN fs >>> arr (evalParallel conf) >>> arr runPar

data BackendType = Master | Slave
type Host = String
type Port = String

startBackend :: RemoteTable -> BackendType -> Host -> Port -> IO (Backend, State)
startBackend remoteTable Master host port = do
	backend <- initializeBackend host port remoteTable

	localNode <- newLocalNode backend

	locState <- initialState localNode
	putMVar localStateMVar locState

	-- fork away the master node
	forkIO $ startMaster backend (master backend)

	-- wait for startup
	waitForStartup locState
	return (backend, locState)
startBackend remoteTable Slave host port = do
	backend <- initializeBackend host port remoteTable

	localNode <- newLocalNode backend

	locState <- initialState localNode
	putMVar localStateMVar locState

	startSlave backend
	return (backend, locState)

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