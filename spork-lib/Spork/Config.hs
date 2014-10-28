{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Spork.Config
  ( readConfig
  ) where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T

import           System.Exit
import           System.IO

import           Database.PostgreSQL.Simple

readConfig :: FromJSON a => FilePath -> IO a
readConfig path = do
  configJson <- BSL.readFile path
  case eitherDecode configJson of
    Right config -> return config
    Left err -> do
      hPutStrLn stderr $ "Can't read the config file: " ++ err
      exitFailure

--dbFromArgs :: ([String]-> DB ()) -> IO
