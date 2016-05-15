{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Spork.Database
  ( readField,
    mReadField,
    mBinaryField,
    DBC(..),
    getConf,
    getConn,
    withConn,
    Connection,
    executeDB, executeManyDB, executeDB_,executeDBres,
    queryIntSetDB, querySetDB, queryMultiSetDB, queryIntMultiSetDB,
    queryDB, queryDB_,queryListDB,
    foldDB,
    foldDB_,
    gfoldDB,
    runDB_io,
    db_ask,
    console,
    consoles,
    conschar,
    liftIO,
    unOnly,
    close,
    Only (..),
    gselectFromDB,
    ginsertIntoDB,
    ginsertManyIntoDB,
    catchDB,
    catchDB_,
    unsafeInterleaveDB,
    memoDB,
    withTransactionDB,
    gselectDB,
    ginsertDB,
    gupdateDB,
    gupsertDB,
    gdeleteDB,
    gfastInsertDB,
    getByKeyDB,
    ginsertNoKeyDB
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           System.IO
import           System.IO.Unsafe

import qualified Data.Binary as B
import Control.Exception
import Control.Applicative
import Data.Monoid ((<>))
import           Generics.SOP
import Data.List (intercalate)
import Data.String (fromString)

import Data.IORef

import           Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as IntMultiSet

import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SOP
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Transaction
import Control.Concurrent.MVar

import  System.Posix.Syslog


newtype DBC conf a = DBC { unDBC :: ReaderT (Connection, conf) IO a }
  deriving (Monad, Functor, MonadIO, Applicative)

db_ask :: DBC conf (Connection, conf)
db_ask = DBC ask

getConf :: DBC conf conf
getConf = DBC $ snd <$> ask

getConn :: DBC conf Connection
getConn = DBC $ fst <$> ask

withConn :: (Connection -> IO a) -> DBC conf a
withConn mx = getConn >>= DBC . lift . mx

executeDB :: ToRow q => Query -> q -> DBC conf ()
executeDB qry args = void $ withConn $ \conn -> execute conn qry args
executeDBres :: ToRow q => Query -> q -> DBC conf Integer
executeDBres qry args = fmap toInteger $ withConn $ \conn -> execute conn qry args

executeManyDB :: ToRow q => Query -> [q] -> DBC conf ()
executeManyDB qry args = void $ withConn $ \conn -> executeMany conn qry args

queryDB :: (ToRow q, FromRow r) => Query -> q -> DBC conf [r]
queryDB qry args = withConn $ \conn -> query conn qry args

executeDB_ :: Query -> DBC conf ()
executeDB_ qry = void $ withConn $ \conn -> execute_ conn qry

queryDB_ :: FromRow r => Query -> DBC conf [r]
queryDB_ qry = withConn $ \conn -> query_ conn qry

foldDB_ :: FromRow r =>
           Query -> a -> (a -> r -> IO a) -> DBC conf a
foldDB_ qry seed f = withConn $ \conn -> fold_ conn qry seed f

foldDB ::  (ToRow params, FromRow row) =>
           Query -> params -> a -> (a -> row -> IO a) -> DBC conf a
foldDB qry pars seed f = withConn $ \conn -> fold conn qry pars seed f

gfoldDB :: forall a r q c. (ToRow q, FromRow r, Generic r, HasFieldNames r) => Query -> q -> a -> (a -> r -> DBC c a) -> DBC c a
gfoldDB q1 args seed f = do
  (conn, conf) <- db_ask
  let fullq = "select " <> (fromString $ intercalate "," $ fieldNames $ (Proxy :: Proxy r) ) <> " from " <> q1
      fopts = defaultFoldOptions { transactionMode = TransactionMode ReadCommitted ReadWrite}
  liftIO $ foldWithOptions fopts conn fullq args seed (\acc row -> runDB_io conn conf $ f acc row)

queryIntSetDB :: ToRow params => Query -> params -> DBC conf IntSet.IntSet
queryIntSetDB q pars = foldDB q pars IntSet.empty $ \(iset) (Only x) -> return $! x `seq` iset `seq` IntSet.insert x iset

querySetDB :: (ToRow params, FromField a, Ord a) => Query -> params -> DBC conf (Set.Set a)
querySetDB q pars = foldDB q pars Set.empty $ \iset (Only x) -> return $! x `seq` iset `seq` Set.insert x iset

queryMultiSetDB ::  (ToRow params, FromField b, Ord a) => Query -> params -> (b -> a) -> DBC conf (MultiSet a)
queryMultiSetDB q pars f = foldDB q pars MS.empty $ \mset (Only x) -> let y = f x
                                                                      in return $! y `seq` MS.insert y mset

queryIntMultiSetDB ::  (ToRow params) => Query -> params -> DBC conf IntMultiSet
queryIntMultiSetDB q pars
  = foldDB q pars IntMultiSet.empty $ \mset (Only x) -> return $! x `seq` IntMultiSet.insert x mset

queryListDB :: (ToRow params, FromField a, Ord a) => Query -> params -> DBC conf [a]
queryListDB q pars = foldDB q pars [] $ \xs (Only x) -> return $! x `seq` (x:xs)

logAlert :: MonadIO m => String -> m ()
logAlert s = liftIO $ do
  syslog System.Posix.Syslog.Alert s

logError :: MonadIO m => String -> m ()
logError s = liftIO $ do
  syslog System.Posix.Syslog.Error s

logWarning :: MonadIO m => String -> m ()
logWarning s = liftIO $ do
  syslog System.Posix.Syslog.Warning s

logNotice :: MonadIO m => String -> m ()
logNotice s = liftIO $ do
  syslog System.Posix.Syslog.Notice s

logInfo :: MonadIO m => String -> m ()
logInfo s = liftIO $ do
  syslog System.Posix.Syslog.Notice s

console :: (MonadIO m, Show a) => String -> a -> m ()
console msg x = liftIO $ do
  putStrLn $ msg++": "++show x
  hFlush stdout

consoles :: (MonadIO m, Show a) => String -> [a] -> m ()
consoles msg xs = liftIO $ do
  putStrLn $ msg++": "
  mapM_ (putStrLn . ("  "++) . show) xs
  hFlush stdout

conschar :: MonadIO m => Char -> m ()
conschar c = liftIO $ putChar c >> hFlush stdout

runDB_io :: Connection -> conf -> DBC conf a -> IO a
runDB_io conn conf mx = runReaderT (unDBC mx) (conn, conf)

catchDB :: DBC conf a -> DBC conf (Either String a)
catchDB f = do
  (conn, conf) <- db_ask
  liftIO $ catch (fmap Right $ runDB_io conn conf f)
                 (\e-> do logWarning $ show (e::SomeException)
                          return $ Left $ show (e::SomeException))
catchDB_ :: DBC conf () -> DBC conf ()
catchDB_ f = do
  (conn, conf) <- db_ask
  liftIO $ catch (void $ runDB_io conn conf f)
                 (\e-> do logWarning $ show (e::SomeException)
                          console "catchDB_error" $ show (e::SomeException))

withTransactionDB :: DBC c a -> DBC c a
withTransactionDB dbx = do
  (conn, conf) <- db_ask
  liftIO $ withTransaction conn $ runDB_io conn conf dbx


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

gselectFromDB qry pars = withConn $ \conn -> gselectFrom conn qry pars

gselectDB qry pars = withConn $ \conn -> gselect conn qry pars

ginsertIntoDB tbl val = withConn $ \conn -> ginsertInto conn tbl val

ginsertDB val = withConn $ \conn -> ginsert conn val

ginsertNoKeyDB val = withConn $ \conn -> ginsertNoKey conn val

getByKeyDB k = withConn $ \conn -> getByKey conn k

ginsertManyIntoDB tbl val = withConn $ \conn -> ginsertManyInto conn tbl val

gdeleteDB val = withConn $ \conn -> gdelete conn val

gupdateDB val = withConn $ \conn -> gupdate conn val

gupsertDB val = withConn $ \conn -> gupsert conn val

gfastInsertDB val = withConn $ \conn -> gfastInsert conn val

unsafeInterleaveDB :: DBC conf a -> DBC conf a
unsafeInterleaveDB mx = do
  (conn, conf) <- db_ask
  liftIO $ unsafeInterleaveIO $ runDB_io conn conf mx

--https://hackage.haskell.org/package/io-memoize-1.0.0.0/docs/src/System-IO-Memoize.html

memoDB :: DBC conf  a -> DBC conf  (DBC conf  a)
memoDB action = do
  memo <- newDBMemoizer
  return (memo action)

newDBMemoizer :: DBC conf (DBC conf a -> DBC conf a)
newDBMemoizer = do
  b <- liftIO $ newMVar True
  r <- liftIO $ newIORef undefined
  return (dbMemoizer b r)

dbMemoizer :: MVar Bool -> IORef a -> DBC conf a -> DBC conf a
dbMemoizer b r action = do
  (conn, conf) <- db_ask
  liftIO $ modifyMVar_ b $ \isEmpty ->
      if isEmpty
        then do v <- runDB_io conn conf action
                writeIORef r v
                return False
        else return False
  liftIO $ readIORef r
