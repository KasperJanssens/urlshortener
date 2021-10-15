{-# LANGUAGE OverloadedStrings #-}

module Server.ServerSpec where

import qualified Client
import           Data.Either               (fromRight, isRight)
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Servant.Client
import           Server.NewUrl             (NewUrl (..))
import           Test.Hspec
import           Test.QuickCheck.Instances ()
import qualified TestUtil

-- Servant client seems to try to resolve the 301, which is surprising. Probably a config option but I do not find it.
-- Therefore, I cannot assert on the content of the location header in this test case it seems.
-- In some way this makes for a full round trip test though, although asserting on the content of
-- google.com is likely not a great idea
spec :: Spec
spec = describe "Server Spec" $ do
  it "Simple server url" $
    TestUtil.withServer $ \(port, _) -> do
      let url = "http://www.google.com"
      Client.waitUntilAvailable port
      manager <- newManager defaultManagerSettings
      let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" port "")
      responseOrError <- runClientM (Client.shorten (NewUrl url)) clientEnv
      responseOrError `shouldSatisfy` isRight
      let shortenedUrl = fromRight undefined responseOrError
      movedPermanentlyOrNoContent <- runClientM (Client.resolve shortenedUrl) clientEnv
      movedPermanentlyOrNoContent `shouldSatisfy` isRight
