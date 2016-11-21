{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Spork.DatabaseConfig
  ( DatabaseConfig(..),
    OnlyDatabaseConfig(..),
    createConn,
    destroyConn
  ) where

import           Control.Exception

import           Data.Aeson

import           GHC.Conc
import           GHC.Generics
import Data.Generics (Typeable, Data)

import           Database.PostgreSQL.Simple

data OnlyDatabaseConfig = OnlyDatabaseConfig { database :: DatabaseConfig } deriving Generic

data DatabaseConfig = DatabaseConfig
  { user                   :: String
  , password               :: String
  , host                   :: String
  , port                   :: Integer
  , dbname                 :: String
  , migrations_directory   :: Maybe String
  , num_stripes            :: Maybe Int
  , res_per_stripe         :: Maybe Int
  } deriving (Show, Eq, Generic, Data,Typeable)

instance FromJSON DatabaseConfig
instance FromJSON OnlyDatabaseConfig

createConn :: DatabaseConfig -> IO Connection
createConn config = do
   catch (createConn' config)
         (\(_::SomeException) -> do putStrLn "Failed to connect to the database, retrying in 10s ..."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' config)


createConn' :: DatabaseConfig -> IO Connection
createConn' config = do
  connect ConnectInfo
    { connectHost     = host     config
    , connectUser     = user     config
    , connectPassword = password config
    , connectDatabase = dbname   config
    , connectPort     = fromInteger $ port config
    }

destroyConn :: Connection -> IO ()
destroyConn = close
