module Spork.Spock where

import Web.Spock

import Spork.DatabaseConfig
import Spork.Database

import Database.PostgreSQL.Simple (Connection)
import           Control.Monad.Reader

getPool :: DatabaseConfig -> PoolOrConn Connection
getPool dbconfig=
 let poolCfg    = PoolCfg 3 1 $ 24*60*60
     pool       = PCConn $ ConnBuilder (createConn dbconfig) destroyConn poolCfg
 in pool

runDBC :: conf -> DBC conf a -> SpockAction Connection sess env a
runDBC conf (DBC r) = runQuery $  \conn-> runReaderT r (conn,conf)
