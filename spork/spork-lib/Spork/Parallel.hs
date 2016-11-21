{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Spork.Parallel where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import           System.Exit
import           System.IO
import           System.Environment

import           Spork.Database
import           Spork.DatabaseConfig

import Data.IORef
import Data.Maybe
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

import qualified System.Posix.Process as SPP


import Control.Concurrent.STM.TBQueue
import qualified GHC.Conc as Conc

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

parMapM :: Int -> (a-> DBC conf b) -> [a] -> DBC conf [b]
parMapM nthreads f xs = do
  (conn, conf) <- db_ask
  liftIO $ do
    chan <-  newTChanIO
    mvxs <- mapM (\x -> (,) <$> newEmptyMVar <*> return x) xs
    atomically $ forM_ mvxs $ \(mv,x) -> do
      writeTChan chan (mv,x)
    let worker =  do
          let doWork = do
                mmvx <- atomically $ mReadTChan chan
                case mmvx of
                  Nothing -> return ()
                  Just (mv,x) -> do catch (do y <- runDB_io conn conf $ f x
                                              putMVar mv $ Just y )
                                           (\e-> do hPutStrLn stdout $ show (e::SomeException)
                                                    putMVar mv Nothing)
                                    doWork
          doWork
    mapM_ (\_-> forkIO worker) [1..nthreads]
    fmap catMaybes $ mapM (takeMVar . fst) mvxs


mReadTChan :: TChan a -> STM (Maybe a)
mReadTChan chan = do
  isEmpty <- isEmptyTChan chan
  if isEmpty
     then return Nothing
     else fmap Just $ readTChan chan

-- intended for postgresql-simple's fold
boundedWorker :: Int -> (a -> DBC c ()) -> DBC c (a -> DBC c (), DBC c ())
boundedWorker nthreads f = do
  (conn, conf) <- db_ask
  liftIO $ do
    q <- newTBQueueIO nthreads
    counter <- newIORef 0
    let worker = do
          let doWork = do
                mx <- atomically $ readTBQueue q
                case mx of
                  Nothing -> return ()
                  Just x -> do () <- catch (runDB_io conn conf $ f x)
                                           (\e-> print (e::SomeException))
                               doWork
          doWork
    mapM_ (\_-> forkIO worker) [1..nthreads]
    let process x = liftIO $ do
           modifyIORef counter (+1)
           counts <- readIORef counter
           when (counts `mod` 100 == 0) $ do
              putStr $ '.':show counts
              hFlush stdout
           liftIO $ atomically $ writeTBQueue q $ Just x
        kill = liftIO $ atomically $ mapM_ (\_-> writeTBQueue q $ Nothing) [1..nthreads]
    return (process, kill)

getCPUs :: DBC c Int
getCPUs = liftIO Conc.getNumCapabilities

forkProcess :: DatabaseConfig -> DBC conf () -> DBC conf ()
forkProcess dbConf task = do
  conf <- getConf
  _ <- liftIO $ SPP.forkProcess $ do
    --http://stackoverflow.com/questions/21178581/how-to-prevent-upstart-from-killing-child-processes-to-a-daemon
    pid <- SPP.getProcessID
    SPP.setProcessGroupIDOf pid pid
    newDBconn <- createConn dbConf
    runDB_io newDBconn conf task
    exitSuccess
  return ()
