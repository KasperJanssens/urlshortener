import qualified Application.UrlShortenerSpec as UrlShortenerSpec
import qualified Database.DatabaseSpec        as DatabaseSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  UrlShortenerSpec.spec
  DatabaseSpec.spec
