{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import Data.List
import Data.String
import Data.Time

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Locale

import Spork.Config
import Spork.Database
import Spork.DatabaseConfig

main :: IO ()
main = dbFromArgs $ \args -> case args of
  "run" : _        -> runMigrations
  "new" : name : _ -> createMigrationFile name
  _ -> liftIO $ do
    hPutStrLn stderr
              "Usage: spork-migrate config-file (run | new migration_name)"
    exitFailure

runMigrations :: DBC OnlyDatabaseConfig ()
runMigrations = do
  alreadyMigratedNames <- getAlreadyMigratedNames

  OnlyDatabaseConfig conf <- getConf
  let migrationsPath = maybe "migrations" id $ migrations_directory conf
  fileNames <- liftIO $ getDirectoryContents migrationsPath

  let toMigrate = sortBy (\(a,_) (b,_) -> compare a b)
                . filter ((".sql" `isSuffixOf`) . snd)
                . filter ((`notElem` alreadyMigratedNames) . fst)
                . map (\n -> (takeWhile (`elem` "0123456789") n
                             , migrationsPath </> n))
                $ fileNames

  forM_ toMigrate $ \(n,p) -> do
    content <- liftIO $ readFile p
    let qStr = content ++ "; INSERT INTO migrations ( name ) VALUES ('"
                       ++ n ++ "')"
        q    = fromString qStr
    executeDB_ q

getAlreadyMigratedNames :: DBC conf [String]
getAlreadyMigratedNames = do
  _ <- executeDB_ $ fromString $ "CREATE TABLE IF NOT EXISTS migrations ("
                                 ++ " name varchar(12) PRIMARY KEY )"

  rows <- queryDB_ "SELECT name FROM migrations"
  return $ map head rows

createMigrationFile :: String -> DBC OnlyDatabaseConfig ()
createMigrationFile name = do
  OnlyDatabaseConfig conf <- getConf
  let migrationsDir = maybe "migrations" id $ migrations_directory conf
  now <- liftIO getCurrentTime
  let prefix   = formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
      fullname = prefix ++ "_" ++ name <.> "sql"
      path     = migrationsDir </> fullname
  liftIO $ writeFile path "-- Enter the SQL queries to execute here.\n"
