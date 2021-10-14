module ShortenerSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Application.UrlShortener as UrlShortener
import Test.QuickCheck (getNonNegative)

spec :: Spec
spec = describe "Shortener Spec" $ do
  prop "base 10 to base 64 and back" $
    \x -> do
      let res = UrlShortener.calculateInBase10 . UrlShortener.calculateInBase62 $ x
      res `shouldBe` x
  prop "shorten and unshorten" $
    \x -> do
      let res = UrlShortener.shortenUrl (getNonNegative x) >>= UrlShortener.shortenedToRowId
      res `shouldBe` Just (getNonNegative x)
