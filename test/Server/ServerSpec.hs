{-# LANGUAGE OverloadedStrings #-}

module Server.ServerSpec where

import           Control.Monad.IO.Class       (liftIO)
import           Data.Either                  (fromLeft, fromRight, isLeft,
                                               isRight)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Conc                     (ThreadId, forkIO, killThread,
                                               threadDelay)
import           GHC.IO                       (bracket)
import           Network.HTTP.Client.Internal (defaultManagerSettings,
                                               newManager)
import qualified Network.Socket.Free          as NetworkFree
import           Servant
import           Servant.Client
import           Server.ApiType
import           Server.NewUrl                (NewUrl (..))
import qualified Server.Server                as Server
import qualified System.Directory             as Dir
import           Test.Hspec
import           Test.Hspec.QuickCheck        (prop)
import           Test.QuickCheck.Instances    ()

databaseName :: Text
databaseName = "myTestDatabase"

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

shorten :: NewUrl -> ClientM Text
resolve :: Text -> ClientM NoContent
isUp :: ClientM NoContent
(shorten :<|> isUp :<|> resolve) = client api

waitUntilAvailable :: Int -> IO ()
waitUntilAvailable port = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM isUp (mkClientEnv manager' (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> do
      threadDelay 1000000
      waitUntilAvailable port
    Right _ -> do
      print "API is available"
      return ()

-- Servant client seems to try to resolve the 301, which is surprising. Probably a config option but I do not find it.
-- Therefore, I cannot assert on the content of the location header in this test case it seems.
-- In some way this makes for a full round trip test though, although asserting on the content of
-- google.com is likely not a great idea 
spec :: Spec
spec = describe "Server Spec" $ do
  it "Simple server url" $
    withServer $ \(port, _) -> do
      let url = "http://www.google.com"
      waitUntilAvailable port
      manager <- newManager defaultManagerSettings
      let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" port "")
      responseOrError <- runClientM (shorten (NewUrl url)) clientEnv
      responseOrError `shouldSatisfy` isRight
      let shortenedUrl = fromRight undefined responseOrError
      movedPermanentlyOrNoContent <- runClientM (resolve shortenedUrl) clientEnv
      movedPermanentlyOrNoContent `shouldSatisfy` isRight
