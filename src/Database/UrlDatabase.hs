module Database.UrlDatabase (insertUrl, findUrl, checkExisting) where

import           Control.Monad.IO.Class  (MonadIO)
import qualified Control.Monad.Logger    as Logger
import           Control.Monad.Reader    (ReaderT)
import           Data.Int                (Int64)
import           Data.Text               (Text)
import           Database.Persist.Sqlite (SqlBackend, SqliteConnectionInfo,
                                          entityKey, (==.))
import qualified Database.Persist.Sqlite as Sqlite
import           Database.Persistence
import           Domain.Url              (Url (..))

--These functions not extremely useful  being so small but I like the clarity the extra types bring
insertUrlAction :: (MonadIO m) => Url -> ReaderT SqlBackend m UrlDBId
insertUrlAction (Url url) = Sqlite.insert (UrlDB url)

getUrlAction :: (MonadIO m) => UrlDBId -> ReaderT SqlBackend m (Maybe UrlDB)
getUrlAction = Sqlite.get

checkExistingAction :: (MonadIO m) => Url -> ReaderT SqlBackend m (Maybe UrlDBId)
checkExistingAction (Url url) = do
  maybeRecord <- Sqlite.selectFirst [UrlDBUrl ==. url] []
  pure $ entityKey <$> maybeRecord

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

checkExisting :: SqliteConnectionInfo -> Url -> IO (Maybe Int64)
checkExisting sqliteConnectionInfo url = do
  maybePersistenceId <- Logger.runStderrLoggingT $
      Sqlite.retryOnBusy $
        Sqlite.runSqliteInfo sqliteConnectionInfo $ do
         checkExistingAction url
  pure $ Sqlite.fromSqlKey <$> maybePersistenceId         
