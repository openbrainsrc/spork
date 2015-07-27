module Spork.Spock where

import Web.Spock.Simple

import Spork.DatabaseConfig
import Spork.Database

import Database.PostgreSQL.Simple (Connection)
import           Control.Monad.Reader
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html (Html)
import Data.Text.Lazy (toStrict)

getPool :: DatabaseConfig -> PoolOrConn Connection
getPool dbconfig=
 let poolCfg    = PoolCfg (maybe 2 id $ num_stripes dbconfig)
                          (maybe 20 id $ res_per_stripe dbconfig)
                          $ 24*60*60
     pool       = PCConn $ ConnBuilder (createConn dbconfig) destroyConn poolCfg
 in pool

runDBC :: conf -> DBC conf a -> SpockAction Connection sess env a
runDBC conf (DBC r) = runQuery $  \conn-> runReaderT r (conn,conf)

blaze :: MonadIO m => Html -> ActionT m a
blaze = html . toStrict . renderHtml
