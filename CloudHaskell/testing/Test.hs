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

instance Trans MaybeGrid

-- remotable declaration for all eval tasks
$(mkEvalTasks [''MaybeGrid, ''MaybeGridList])
$(mkRemotables [''MaybeGrid, ''MaybeGridList])
$(mkEvaluatables [''MaybeGrid, ''MaybeGridList])

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      conf <- initializeMaster myRemoteTable host port
      -- wait a bit
      --threadDelay 1000000
      readMVar (workers conf) >>= print

      grids <- fmap lines $ readFile "sudoku.txt"

      print (length (filter isJust (farm conf 4 solve grids)))

      -- TODO: actual computation here!
    ["slave", host, port] -> do
      initializeSlave myRemoteTable host port
      print "slave shutdown."
