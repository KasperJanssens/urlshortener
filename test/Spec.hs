import qualified Application.UrlShortenerSpec as UrlShortenerSpec
import qualified Database.DatabaseSpec        as DatabaseSpec
import qualified Server.ServerSpec            as ServerSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  ServerSpec.spec
  UrlShortenerSpec.spec
  DatabaseSpec.spec
