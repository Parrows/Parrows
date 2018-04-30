{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import GHC.Generics (Generic)

import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar

import System.Environment (getArgs)

import Parrows.CloudHaskell.SimpleLocalNet

import Parrows.Definition
import Parrows.Skeletons.Map

import Parrows.Future hiding (put', get')
import Parrows.Util

import Control.Arrow

import Control.DeepSeq

import Control.Distributed.Process.Node(runProcess)
import System.IO.Unsafe(unsafePerformIO)

-- for Sudoku

import Sudoku
import Data.Maybe

type MaybeGrid = Maybe Grid
type MaybeGridList = [Maybe Grid]
type FutureGrid = CloudFuture (Maybe Grid)
type FutureGridList = [CloudFuture (Maybe Grid)]

-- remotable declaration for all eval tasks
$(mkEvalTasks [''MaybeGrid, ''MaybeGridList, ''FutureGrid, ''FutureGridList])
$(mkRemotables [''MaybeGrid, ''MaybeGridList, ''FutureGrid, ''FutureGridList])
$(mkEvaluatables [''MaybeGrid, ''MaybeGridList, ''FutureGrid, ''FutureGridList])

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      (backend, state) <- startBackend myRemoteTable Master host port

      conf <- defaultConf

      grids <- fmap lines $ readFile "sudoku.txt"

      --let inputs = rnf grids `seq` map (Parrows.Future.put conf) grids

      --print (length (filter isJust $ map (Parrows.Future.get conf) (farm conf 4 (liftFut conf solve) inputs)))

      print (length (filter isJust $ (farm conf 4 solve grids)))

      -- TODO: actual computation here!
    ["slave", host, port] -> do
      startBackend myRemoteTable Slave host port
      print "slave shutdown."
