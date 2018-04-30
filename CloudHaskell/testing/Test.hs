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
type FutureGridList = CloudFuture [Maybe Grid]

-- remotable declaration for all eval tasks
$(mkEvalTasks [''MaybeGrid, ''MaybeGridList, ''FutureGridList])
$(mkRemotables [''MaybeGrid, ''MaybeGridList, ''FutureGridList])
$(mkEvaluatables [''MaybeGrid, ''MaybeGridList, ''FutureGridList])

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      conf <- startBackend myRemoteTable Master host port

      grids <- fmap lines $ readFile "sudoku.txt"

      let inputs = rnf grids `seq` map (put conf) grids

      print (length (filter isJust $ map (get conf) (farm conf 4 (liftFut conf solve) grids)))

      -- TODO: actual computation here!
    ["slave", host, port] -> do
      startBackend myRemoteTable Slave host port
      print "slave shutdown."
