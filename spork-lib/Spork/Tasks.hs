{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Spork.Tasks
  ( taskRunner
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import           System.Exit
import           System.IO
import           System.Environment
import           System.Process

import           Spork.Database
import           Spork.Config
import           Spork.DatabaseConfig
import  System.Posix.Syslog
import Control.Exception
import Control.Monad (when)



taskRunner :: FromJSON c => [(String, [String] -> DBC c ())] -> IO ()
taskRunner subcmds = do
  progName <- getProgName
  allargs <- getArgs
  case allargs of
      [] -> liftIO $ putStrLn "first argument should be a JSON config file"
      (confnm:args) -> do
        catch ( do OnlyDatabaseConfig dbconf <- readConfig confnm
                   conn <- if ("--disable-database-connection" `elem` args)
                             then return undefined
                             else createConn dbconf
                   allconf <- readConfig confnm
                   runDB_io conn allconf $ dispatch args dbconf subcmds
                   when (not $ "--disable-database-connection" `elem` args) $
                     destroyConn conn )
              (\e -> do hPutStrLn stderr $ show (e::SomeException)
                        exitWith $ ExitFailure 1 )

dispatch ("psql":rest) dbconf subcmds = do
  let cmd = "PGPASSWORD="++password dbconf ++" psql -U "++user dbconf ++" "++dbname dbconf

  liftIO $ putStrLn cmd >> system cmd
  return ()

dispatch (subcmd:rest) dbconf subcmds = case lookup subcmd subcmds of
   Just f -> f rest
   Nothing -> help subcmds

dispatch [] dbconf subcmds
  = help subcmds

help subcmds = liftIO $ do
  putStrLn $ "Avaliable commands: "
  mapM_ (\(subcmd,_)-> putStrLn $ "  "++subcmd ) subcmds
