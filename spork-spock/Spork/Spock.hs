module Spork.Spock where

import Web.Spock
import Spork.DatabaseConfig
import Database.PostgreSQL.Simple (Connection)

getPool :: DatabaseConfig -> PoolOrConn Connection
getPool dbconfig=
 let poolCfg    = PoolCfg 3 1 $ 24*60*60
     pool       = PCConn $ ConnBuilder (createConn dbconfig) destroyConn poolCfg
 in pool
