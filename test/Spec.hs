import qualified DatabaseSpec
import qualified ShortenerSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  ShortenerSpec.spec
  DatabaseSpec.spec
