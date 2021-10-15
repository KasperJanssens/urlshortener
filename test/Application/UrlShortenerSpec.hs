module Application.UrlShortenerSpec where

import qualified Application.UrlShortener  as UrlShortener
import           Data.Maybe                (fromJust, isJust)
import qualified TestUtil
import           Domain.Url                (Url (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "Shortener Spec" $ do
  prop "base 10 to base 64 and back" $
    \x -> do
      let res = UrlShortener.calculateInBase10 . UrlShortener.calculateInBase62 $ x
      res `shouldBe` x
  prop "shorten and unshorten" $
    \randomUrl -> TestUtil.withDatabase $ \sqliteConnectionInfo -> do
      shortenedUrl <- UrlShortener.shortenUrl sqliteConnectionInfo (Url randomUrl)
      shortenedUrl `shouldSatisfy` isJust
      resolvedUrl <- UrlShortener.resolveUrl sqliteConnectionInfo $ fromJust shortenedUrl
      resolvedUrl `shouldBe` Just randomUrl
