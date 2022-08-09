import           Test.Hspec

import qualified Money.Systems.Superfluid.RealTimeBalance_prop

main :: IO ()
main = hspec $ do
    Money.Systems.Superfluid.RealTimeBalance_prop.tests
