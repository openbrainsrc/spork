{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Spork.Config
  ( readConfig,
    dbFromArgs
  ) where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T

import           System.Exit
import           System.IO
import           System.Environment

import           Database.PostgreSQL.Simple

import           Spork.Database
import           Spork.DatabaseConfig

readConfig :: FromJSON a => FilePath -> IO a
readConfig path = do
  configJson <- BSL.readFile path
  case eitherDecode configJson of
    Right config -> return config
    Left err -> do
      hPutStrLn stderr $ "Can't read the config file: " ++ err
      exitFailure

dbFromArgs :: FromJSON conf => ([String]-> DB conf ()) -> IO ()
dbFromArgs f = do
  (confnm:args) <- getArgs
  OnlyDatabaseConfig dbconf <- readConfig confnm
  conn <- createConn dbconf
  allconf <- readConfig confnm
  runDB_io conn allconf $ f args
