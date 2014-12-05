{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Spork.Parallel where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import           System.Exit
import           System.IO
import           System.Environment

import           Spork.Database
import           Spork.DatabaseConfig

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

parMapM_ :: Int -> (a-> DBC conf b) -> [a] -> DBC conf [b]
parMapM_ nthreads f xs = do
  chan <- liftIO $ newTChanIO
  liftIO $ atomically $ writeTChan chan xs
  (conn, conf) <- db_ask
  let worker = runDB_io conn conf $ do
       return ()
  return []
