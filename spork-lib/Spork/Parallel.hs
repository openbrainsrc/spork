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
import Control.Concurrent
import Control.Concurrent.MVar

parMapM_ :: Int -> (a-> DBC conf ()) -> [a] -> DBC conf ()
parMapM_ nthreads f xs = do
  chan <- liftIO $ newTChanIO
  liftIO $ atomically $ mapM_ (writeTChan chan) xs
  (conn, conf) <- db_ask
  locks <- liftIO $ sequence $ replicate nthreads newEmptyMVar
  let worker lock = runDB_io conn conf $ do
        let doWork = do
              mval <- mReadTChan chan
              case mval of
                Nothing -> return ()
                Just x -> do () <- f x
                             doWork
        doWork
        liftIO $ putMVar lock ()

  liftIO $ mapM_ (forkIO . worker) locks
  liftIO $ mapM_ takeMVar locks

mReadTChan :: TChan a -> DBC conf (Maybe a)
mReadTChan chan = liftIO $ atomically $ do
  isEmpty <- isEmptyTChan chan
  if isEmpty
     then return Nothing
     else fmap Just $ readTChan chan
