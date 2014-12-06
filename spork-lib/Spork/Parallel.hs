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
import Control.Exception
import Control.Concurrent.MVar

parMapM_ :: Int -> (a-> DBC conf ()) -> [a] -> DBC conf ()
parMapM_ nthreads f xs = do
  (conn, conf) <- db_ask
  liftIO $ do
    chan <-  newTChanIO
    atomically $ mapM_ (writeTChan chan) xs
    locks <-  sequence $ replicate nthreads newEmptyMVar
    let worker lock =  do
          let doWork = do
                mval <- atomically $ mReadTChan chan
                case mval of
                  Nothing -> return ()
                  Just x -> do () <- catch (runDB_io conn conf $ f x)
                                           (\e-> print (e::SomeException))
                               doWork
          doWork
          putMVar lock ()

    mapM_ (forkIO . worker) locks
    mapM_ takeMVar locks

mReadTChan :: TChan a -> STM (Maybe a)
mReadTChan chan = do
  isEmpty <- isEmptyTChan chan
  if isEmpty
     then return Nothing
     else fmap Just $ readTChan chan
