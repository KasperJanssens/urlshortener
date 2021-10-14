module Database.UrlDatabase (insertUrl, findUrl) where

import           Control.Monad.IO.Class  (MonadIO)
import qualified Control.Monad.Logger    as Logger
import           Control.Monad.Reader    (ReaderT)
import           Data.Int                (Int64)
import           Data.Text               (Text)
import           Database.Persist.Sqlite (SqlBackend, SqliteConnectionInfo)
import qualified Database.Persist.Sqlite as Sqlite
import           Database.Persistence    (UrlDB (..), UrlDBId)
import           Domain.Url              (Url (..))

--Both functions not extremely useful but I like the clarity the extra types bring
insertUrlAction :: (MonadIO m) => Url -> ReaderT SqlBackend m UrlDBId
insertUrlAction (Url url) = Sqlite.insert (UrlDB url)

getUrlAction :: (MonadIO m) => UrlDBId -> ReaderT SqlBackend m (Maybe UrlDB)
getUrlAction = Sqlite.get

insertUrl :: SqliteConnectionInfo -> Url -> IO Int64
insertUrl sqliteConnectionInfo newUrl = do
  persistenceId <- Logger.runStderrLoggingT $
    Sqlite.retryOnBusy $
      Sqlite.runSqliteInfo sqliteConnectionInfo $ do
        insertUrlAction newUrl
  return $ Sqlite.fromSqlKey persistenceId

findUrl :: SqliteConnectionInfo -> Int64 -> IO (Maybe Text)
findUrl sqliteConnectionInfo key = do
  let urlDbKey = Sqlite.toSqlKey key
  maybeUrlDb <-
    Logger.runStderrLoggingT $
      Sqlite.retryOnBusy $
        Sqlite.runSqliteInfo sqliteConnectionInfo $ getUrlAction urlDbKey
  return $ urlDBUrl <$> maybeUrlDb
