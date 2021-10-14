{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where

import           Control.Exception         (bracket)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Database.Persist.Sqlite   (SqliteConnectionInfo,
                                            mkSqliteConnectionInfo)
import qualified Database.Persistence      as Persistence
import qualified Database.UrlDatabase      as UrlDatabase
import           Domain.Url                (Url (..))
import qualified System.Directory          as Dir
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

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

--This is kinda annoying but I cannot seem to get `around` to work with a quick check property.
--It seems to be limited to Spec and I think it needs SpecWith a. The result is that the database
--Is migrated for every run of the prop instead of once.
spec :: Spec
spec = describe "Database Spec" $ do
  prop "Database prop" $
    \randomUrl -> withDatabase $ \sqliteConnectionInfo -> do
      shortened <- UrlDatabase.insertUrl sqliteConnectionInfo (Url randomUrl)
      res <- UrlDatabase.findUrl sqliteConnectionInfo shortened
      res `shouldBe` Just randomUrl
