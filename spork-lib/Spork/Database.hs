{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Spork.Database
  ( readField,
    mReadField,
    mBinaryField,
    DB,
    withConn,
    Connection,
    executeDB, executeManyDB, executeDB_,
    queryDB, queryDB_,
    foldDB,
    foldDB_,
    unDB,
    runDB_io,
    db_ask,
    console,
    consoles,
    liftIO,
    unOnly,
    close,
    Only (..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T

import           System.Exit
import           System.IO

import           GHC.Conc
import GHC.Generics

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Control.Monad.Reader
import Control.Monad (void)
import qualified Data.Binary as B

newtype DB conf a = DB { unDB :: ReaderT (Connection, conf) IO a }
  deriving (Monad, Functor, MonadIO)

db_ask :: DB conf (Connection, conf)
db_ask = DB ask

withConn :: (Connection -> IO a) -> DB conf a
withConn mx = db_ask >>= \(conn,_) -> DB (lift $ mx conn)

executeDB :: ToRow q => Query -> q -> DB conf ()
executeDB qry args = void $ withConn $ \conn -> execute conn qry args

executeManyDB :: ToRow q => Query -> [q] -> DB conf ()
executeManyDB qry args = void $ withConn $ \conn -> executeMany conn qry args

queryDB :: (ToRow q, FromRow r) => Query -> q -> DB conf [r]
queryDB qry args = withConn $ \conn -> query conn qry args

executeDB_ :: Query -> DB conf ()
executeDB_ qry = void $ withConn $ \conn -> execute_ conn qry

queryDB_ :: FromRow r => Query -> DB conf [r]
queryDB_ qry = withConn $ \conn -> query_ conn qry

foldDB_ :: FromRow r =>
           Query -> a -> (a -> r -> IO a) -> DB conf a
foldDB_ qry seed f = withConn $ \conn -> fold_ conn qry seed f

foldDB ::  (ToRow params, FromRow row) =>
           Query -> params -> a -> (a -> row -> IO a) -> DB conf a
foldDB qry pars seed f = withConn $ \conn -> fold conn qry pars seed f

console :: (MonadIO m, Show a) => String -> a -> m ()
console msg x = liftIO $ putStrLn $ msg++": "++show x

consoles :: (MonadIO m, Show a) => String -> [a] -> m ()
consoles msg xs = liftIO $ do
  putStrLn $ msg++": "
  mapM_ (putStrLn . ("  "++) . show) xs

runDB_io :: Connection -> conf -> DB conf a -> IO a
runDB_io conn conf mx = runReaderT (unDB mx) (conn, conf)

unOnly :: Only a -> a
unOnly (Only x) = x

readField :: Read a => RowParser a
readField = do
  str <- field
  case reads str of
    (x,_) : _ -> return x
    _         -> fail $ "readField: malformed data: " ++ str

mReadField :: Read a => RowParser (Maybe a)
mReadField = do
  mstr <- field
  case fmap reads mstr of
    Just ((x,_) : _) -> return $ Just x
    _ -> return Nothing

mBinaryField :: B.Binary a => RowParser (Maybe a)
mBinaryField = do
  mbs <- field
  case mbs of
    Nothing -> return Nothing
    Just bs -> case B.decodeOrFail bs of
                 Left _ -> return Nothing
                 Right (_,_,x) -> return $ Just x
