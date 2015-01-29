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


taskRunner :: FromJSON c => [(String, [String] -> DBC c ())] -> IO ()
taskRunner subcmds = do
  (confnm:args) <- getArgs
  OnlyDatabaseConfig dbconf <- readConfig confnm
  conn <- createConn dbconf
  allconf <- readConfig confnm
  runDB_io conn allconf $ dispatch args dbconf subcmds

dispatch ("psql":rest) dbconf subcmds = do
  let cmd = "PGPASSWORD="++password dbconf ++" psql -U "++user dbconf ++" "++dbname dbconf

  liftIO $ putStrLn cmd >> system cmd
  return ()

dispatch (subcmd:rest) dbconf subcmds = case lookup subcmd subcmds of
   Just f -> f rest
   Nothing -> liftIO $ putStrLn $ "Avaliable commands: "++show (map fst subcmds)
