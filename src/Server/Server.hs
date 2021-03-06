{-# LANGUAGE OverloadedStrings #-}

module Server.Server where

import qualified Application.UrlShortener  as UrlShortener
import           Control.Monad.Reader
import           Data.Coerce               (coerce)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as TextEncoding
import           Database.Persist.Sqlite   (SqliteConnectionInfo)
import qualified Database.Persist.Sqlite   as Sqlite
import qualified Database.Persistence      as Persistence
import qualified Database.UrlDatabase      as UrlDatabase
import           Domain.ShortenedUrl       (ShortenedUrl (..))
import           Domain.Url                (Url (..))
import qualified Network.HTTP.Types.Header as Header
import           Network.Wai.Handler.Warp  (run)
import           Servant                   (Application, Handler, Server)
import qualified Servant
import           Servant.API
import           Server.ApiType            (ResolverApi, ShortenApi,
                                            ShortenerAPI, UpApi, api)
import           Server.NewUrl             (NewUrl (..))



postUrlToShorten :: SqliteConnectionInfo -> NewUrl -> Handler Text
postUrlToShorten sqliteConnectionInfo (NewUrl url) = do
  maybeShortenedUrl <- liftIO $ UrlShortener.shortenUrl sqliteConnectionInfo (Url url)
  shortenedUrl <- maybe (Servant.throwError Servant.err404 {Servant.errBody = "Cannot shorten url"}) pure maybeShortenedUrl
  return $ coerce shortenedUrl

serveShorten :: SqliteConnectionInfo -> Server ShortenApi
serveShorten = postUrlToShorten

resolveUrl :: SqliteConnectionInfo -> Text -> Handler NoContent
resolveUrl sqliteConnectionInfo shortUrl =
  do
    maybeStoredUrl <- liftIO $ UrlShortener.resolveUrl sqliteConnectionInfo (coerce shortUrl)
    storedUrl <- maybe (Servant.throwError Servant.err404 {Servant.errBody = "Unknown short url"}) pure maybeStoredUrl
    let locationHeader = (Header.hLocation, TextEncoding.encodeUtf8 storedUrl)
    Servant.throwError $ Servant.err301 {Servant.errHeaders = [locationHeader]}

serveResolve :: SqliteConnectionInfo -> Server ResolverApi
serveResolve = resolveUrl

serveUp :: Server UpApi
serveUp = return NoContent

server :: SqliteConnectionInfo -> Server ShortenerAPI
server sqliteConnectionInfo = serveShorten sqliteConnectionInfo :<|> serveUp :<|> serveResolve sqliteConnectionInfo

application :: SqliteConnectionInfo -> Application
application sqliteConnectionInfo req respond = do
  Servant.serve
    api
    (server sqliteConnectionInfo)
    req
    respond

start :: Int -> Text -> IO ()
start port databaseName =
  let sqliteConnectionInfo = Sqlite.mkSqliteConnectionInfo databaseName
   in do
        Persistence.initDatabase sqliteConnectionInfo
        run port $ application sqliteConnectionInfo
