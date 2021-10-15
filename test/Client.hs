module Client where

import           Data.Text                    (Text)
import           Network.HTTP.Client.Internal (defaultManagerSettings,
                                               newManager)
import           Servant
import           Servant.Client
import           Server.ApiType               (api)
import           Server.NewUrl                (NewUrl)
import           GHC.Conc                     (threadDelay)

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
