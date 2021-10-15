{-# LANGUAGE OverloadedStrings #-}

module TestUtil where

import           Control.Exception       (bracket)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Database.Persist.Sqlite (SqliteConnectionInfo,
                                          mkSqliteConnectionInfo)
import qualified Database.Persistence    as Persistence
import qualified System.Directory        as Dir
import qualified Server.Server                as Server
import qualified Network.Socket.Free          as NetworkFree
import           GHC.Conc                     (ThreadId, forkIO, killThread)

databaseName :: Text
databaseName = "myTestDatabase"

setupDatabase :: IO SqliteConnectionInfo
setupDatabase = do
  let sqliteConnectionInfo = mkSqliteConnectionInfo databaseName
  Persistence.initDatabase sqliteConnectionInfo
  return sqliteConnectionInfo

teardownDatabase :: SqliteConnectionInfo -> IO ()
teardownDatabase _ = do
  filePath <- Dir.makeAbsolute $ Text.unpack databaseName
  Dir.removeFile filePath

withDatabase :: (SqliteConnectionInfo -> IO ()) -> IO ()
withDatabase = bracket setupDatabase teardownDatabase


setupServer :: IO (Int, ThreadId)
setupServer = do
  port <- NetworkFree.getFreePort
  threadId <- forkIO $ Server.start port databaseName
  return (port, threadId)

teardownServer :: (Int, ThreadId) -> IO ()
teardownServer (_, threadId) = do
  killThread threadId
  filePath <- Dir.makeAbsolute $ Text.unpack databaseName
  Dir.removeFile filePath

withServer :: ((Int, ThreadId) -> IO ()) -> IO ()
withServer = bracket setupServer teardownServer