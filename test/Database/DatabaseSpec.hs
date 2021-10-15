{-# LANGUAGE OverloadedStrings #-}

module Database.DatabaseSpec where

import           Data.Maybe                (isNothing)
import qualified Database.UrlDatabase      as UrlDatabase
import qualified TestUtil
import           Domain.Url                (Url (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

--This is kinda annoying but I cannot seem to get `around` to work with a quick check property.
--It seems to be limited to Spec and I think it needs SpecWith a. The result is that the database
--Is migrated for every run of the prop instead of once and all tests run against it.
spec :: Spec
spec = describe "Database Spec" $ do
  prop "Database prop" $
    \randomUrl -> TestUtil.withDatabase $ \sqliteConnectionInfo -> do
      shortened <- UrlDatabase.insertUrl sqliteConnectionInfo (Url randomUrl)
      res <- UrlDatabase.findUrl sqliteConnectionInfo shortened
      res `shouldBe` Just randomUrl
  it "find without inserting should return Nothing" $
    TestUtil.withDatabase $ \sqliteConnectionInfo -> do
      res <- UrlDatabase.findUrl sqliteConnectionInfo 0
      res `shouldSatisfy` isNothing
