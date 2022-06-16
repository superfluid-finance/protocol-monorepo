import           Test.Framework                        (Test, defaultMain, testGroup)

import qualified Money.Systems.Superfluid.RealtimeBalance_prop
import qualified Money.Systems.Superfluid.System_unit

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "RealtimeBalance prop"  Money.Systems.Superfluid.RealtimeBalance_prop.tests
    , testGroup "System unit" Money.Systems.Superfluid.System_unit.tests
    ]
