import           Test.Hspec

import qualified Money.Systems.Superfluid.RealTimeBalance_prop
import qualified Money.Systems.Superfluid.System_test

main :: IO ()
main = hspec $ do
    Money.Systems.Superfluid.RealTimeBalance_prop.tests
    Money.Systems.Superfluid.System_test.tests
