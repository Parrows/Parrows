{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import GHC.Generics (Generic)

import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar

import System.Environment (getArgs)

import Parrows.CloudHaskell.SimpleLocalNet
import Parrows.CloudHaskell.EvalGen

import Parrows.Definition
import Parrows.Skeletons.Map

import Parrows.Future
import Parrows.Util

import Control.Arrow

import Control.DeepSeq

-- for Sudoku

import Sudoku
import Data.Maybe

evalTaskInt :: (SendPort (SendPort (Thunk Int)), SendPort Int) -> Process ()
evalTaskInt = evalTaskBase

data MyInt = I {-# NOUNPACK #-} Int deriving (Show, Generic, Typeable)

instance Binary MyInt
instance NFData MyInt where
  rnf (I x) = rnf $ x

-- welp. our template haskell stuff only works for 
-- Names for now. but it works
type MaybeGrid = Maybe Grid
type MaybeGridList = [Maybe Grid]

-- remotable declaration for all eval tasks
$(mkEvalTasks [''MyInt, ''Int, ''MaybeGrid, ''MaybeGridList])
$(mkRemotables [''MyInt, ''Int, ''MaybeGrid, ''MaybeGridList])
$(mkEvaluatables [''MyInt, ''Int, ''MaybeGrid, ''MaybeGridList])

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

fib (I n) = I $ go n (0,1)
  where
    go !n (!a, !b) | n==0      = a
                   | otherwise = go (n-1) (b, a+b)

parFib :: Conf -> [MyInt] -> [MyInt]
parFib conf xs = parEvalN conf (repeat fib) $ xs

instance (NFData a, Evaluatable b, ArrowChoice arr) => ArrowParallel arr a b Conf where
    parEvalN conf fs = arr (force) >>> evalN fs >>> arr (evalParallel conf) >>> arr runPar

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port myRemoteTable

      localNode <- newLocalNode backend

      conf <- defaultInitConf localNode

      -- fork away the master node
      forkIO $ startMaster backend (master conf backend)

      -- wait for startup
      waitForStartup conf

      -- wait a bit
      --threadDelay 1000000
      readMVar (workers conf) >>= print

      grids <- fmap lines $ readFile "sudoku.txt"

      print (length (filter isJust (farm conf 4 solve grids)))

      -- TODO: actual computation here!
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
