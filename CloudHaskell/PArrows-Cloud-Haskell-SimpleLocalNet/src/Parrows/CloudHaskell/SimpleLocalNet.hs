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

  PipeIn(..),
  PipeOut(..),
  Trans(..),

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

type PipeIn a = SendPort (SendPort (Maybe (SendPort (Maybe a))))
type PipeOut a = ReceivePort (SendPort (Maybe (SendPort (Maybe a))))

createC :: (Binary a, Typeable a) => Process (PipeIn a, PipeOut a)
createC = newChan

readSingle :: (Binary a, Typeable a) => ReceivePort (Maybe a) -> Process (Maybe a)
readSingle recPort = receiveChan recPort

connectAsSender :: (Typeable a, Binary a) => PipeIn a -> Process [SendPort (Maybe a)]
connectAsSender pipeIn = do
  (sp, cin) <- newChan
  -- liftIO $ putStrLn $ "pre send (connectAsSender) to pipeIn"
  sendChan pipeIn sp
  -- liftIO $ putStrLn $ "pre getAllSenders"
  getAllSenders cin []

connectAsReceiver :: (Typeable a, Binary a) => Int -> PipeOut a -> Process [ReceivePort (Maybe a)]
connectAsReceiver cnt pipeOut = do 
  spsp <- receiveChan pipeOut
  retVal <- sequence $ replicate cnt (sendSingleSendPort spsp)
  sendChan spsp Nothing
  return retVal

sendSingleSendPort :: (Binary a, Typeable a) =>
  (SendPort (Maybe (SendPort (Maybe a)))) ->
  Process (ReceivePort (Maybe a))
sendSingleSendPort spsp = do
  (sp, rp) <- newChan
  -- liftIO $ putStrLn $ "sending back sp" ++ (show spsp)
  sendChan spsp (Just sp)
  -- liftIO $ putStrLn $ "sent back sp " ++ (show sp)
  return rp

connectAsReceiverSingle :: (Binary a, Typeable a) => PipeOut a -> Process (ReceivePort (Maybe a))
connectAsReceiverSingle pipeOut = do
  retVals <- connectAsReceiver 1 pipeOut
  return $ head retVals

getAllSenders :: (Typeable a, Binary a) =>
  ReceivePort (Maybe (SendPort (Maybe a))) ->
  [SendPort (Maybe a)] ->
  Process [SendPort (Maybe a)]
getAllSenders sp vals = do 
  x <- readSingle sp
  case x of
      Just val -> getAllSenders sp (val : vals)
      Nothing -> return vals


class (Typeable a, Binary a, NFData a) => Trans a where
  writeChan :: PipeIn a -> a -> Process ()
  writeChan pipeIn x = do
    sps <- connectAsSender pipeIn
    let sp = head sps  
    sendVal sp x    

  sendVal :: SendPort (Maybe a) -> a -> Process ()
  sendVal sp x = rnf x `seq` sendChan sp $ Just x

  readChan :: PipeOut a -> Process a
  readChan pipeOut = do
      rp <- connectAsReceiverSingle pipeOut
      readVal rp

  readVal :: ReceivePort (Maybe a) -> Process a
  readVal rp = do
    val <- readSingle rp
    case val of
      Just x -> return x
      Nothing -> error "expected value"

instance (Typeable a, Binary a) => Trans (CloudFuture a)

instance (Typeable a, Binary a) => Binary (CloudFuture a) where
    put (CF sp) = Data.Binary.put sp
    get = do
        val <- Data.Binary.get
        return $ CF val

dtt :: a
dtt = error "don't touch this!"

instance (Trans a, Trans b, Trans c) => Trans (a, b, c) where
  writeChan pipeIn (a, b, c) = do
    [spA, spB, spC] <- connectAsSender pipeIn
    liftIO $ forkProcess (localNode ownLocalConf) $ relaySP a (\a -> (a, dtt, dtt)) spA
    liftIO $ forkProcess (localNode ownLocalConf) $ relaySP b (\b -> (dtt, b, dtt)) spB
    relaySP c (\b -> (dtt, dtt, c)) spC

  -- only the outermost tuple has proper streaming, so no sendVal or readVal 
  -- sendVal sp x = ..
  -- readVal rp = ...

  readChan pipeOut = do
    [rpA, rpB, rpC] <- connectAsReceiver 3 pipeOut
    a <- relayRP rpA (\(a, _, _) -> a)
    b <- relayRP rpA (\(_, b, _) -> b)
    c <- relayRP rpA (\(_, _, c) -> c)
    return (a, b, c)


relaySP :: (Trans a, Trans b) => a -> (a -> b) -> SendPort (Maybe b) -> Process ()
relaySP a f out = do
  (sp, rp) <- newChan
  sendVal sp a
  againA <- readVal rp
  let b = f againA
  sendVal out b

relayRP :: (Trans a, Trans b) => ReceivePort (Maybe b) -> (b -> a) -> Process a
relayRP rp f = do
  b <- readVal rp
  let a = f b
  return a

instance (Trans a) => Trans [a] where
  sendVal sp x = do
    sequence_ $ map (\val -> rnf val `seq` sendChan sp $ Just $ [val]) x
    sendChan sp Nothing

  readVal rp = do
    readUntilNothing rp []
      where
        readUntilNothing :: ReceivePort (Maybe [a]) -> [a] -> Process [a]
        readUntilNothing rp vals = do 
          x <- readSingle rp
          case x of
              Just val -> readUntilNothing rp ((head val) : vals)
              Nothing -> return vals

class (Trans a, Binary a, Typeable a, NFData a) => Evaluatable a where
  evalTask :: (SendPort (SendPort (Thunk a)), PipeIn a) -> Closure (Process ())

-- | base evaluation task for easy instance declaration
evalTaskBase :: (Trans a, Binary a, Typeable a, NFData a) => 
  (SendPort (SendPort (Thunk a)), PipeIn a) -> Process ()
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
  writeChan output a

-- | forces a single value
forceSingle :: (Evaluatable a) => Conf -> NodeId -> MVar a -> a -> Process ()
forceSingle conf node out a = do
  -- create the Channel that we use to send the 
  -- Sender of the input from the slave node from
  (inputSenderSender, inputSenderReceiver) <- newChan

  -- create the channel to receive the output from
  (outputSender, outputReceiver) <- createC

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
  forcedA <- readChan outputReceiver

  -- put the output back into the passed MVar
  liftIO $ putMVar out forcedA

-- | evaluates a single value
evalSingle :: Evaluatable a => Conf -> NodeId -> a -> IO (Computation a)
evalSingle conf node a = do
  mvar <- newEmptyMVar
  let computation = forkProcess (localNode conf) $ forceSingle conf node mvar a
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


isDebug :: Bool
isDebug = True

debug :: (Show a) => a -> Process ()
debug a = if isDebug then liftIO $ putStrLn $ show a else return ()

{-# NOINLINE put' #-}
put' :: (Binary a, Typeable a) => Conf -> a -> CloudFuture a
put' conf a = unsafePerformIO $ do
    mvar <- newEmptyMVar
    forkProcess (localNode ownLocalConf) $ do
      (senderSender, senderReceiver) <- newChan
      debug $ (typeOf senderSender)
      liftIO $ putMVar mvar senderSender
      debug $ "put mvar"
      debug $ "pre receive sender"
      debug $ "type: " ++ (show $ typeOf $ senderReceiver)
      sender <- receiveChan senderReceiver
      debug $ "received sender"
      sendChan sender a
      debug $ "sent"
    takeMVar mvar >>= (return . CF)

{-# NOINLINE get' #-}
get' :: (Binary a, Typeable a) => Conf -> CloudFuture a -> a
get' conf (CF senderSender) = unsafePerformIO $ do
    mvar <- newEmptyMVar
    forkProcess (localNode ownLocalConf) $ do
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