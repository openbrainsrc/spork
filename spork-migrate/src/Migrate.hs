{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import Data.List
import Data.String

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Spork.Config
import Spork.Database
import Spork.DatabaseConfig

main :: IO ()
main = dbFromArgs $ \_ -> do
  alreadyMigratedTimestamps <- getAlreadyMigratedTimestamps

  OnlyDatabaseConfig conf <- getConf
  let migrationsPath = maybe "migrations" id $ migrations_directory conf
  fileNames <- liftIO $ getDirectoryContents migrationsPath

  let toMigrate = sortBy (\(a,_) (b,_) -> compare a b)
                . filter ((".sql" `isSuffixOf`) . snd)
                . filter ((`notElem` alreadyMigratedTimestamps) . fst)
                . map (\n -> (takeWhile (`elem` "0123456789") n
                             , migrationsPath </> n))
                $ fileNames

  forM_ toMigrate $ \(t,p) -> do
    content <- liftIO $ readFile p
    let qStr = content ++ "; INSERT INTO migrations ( timestamp ) VALUES ('"
                       ++ t ++ "')"
        q    = fromString qStr
    executeDB_ q

getAlreadyMigratedTimestamps :: DBC conf [String]
getAlreadyMigratedTimestamps = do
  _ <- executeDB_ $ fromString $ "CREATE TABLE IF NOT EXISTS migrations ("
                                 ++ " timestamp varchar(12) NOT NULL )"

  rows <- queryDB_ "SELECT timestamp FROM migrations"
  return $ map head rows
