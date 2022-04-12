import           Test.Framework                        (Test, defaultMain, testGroup)

import qualified Money.Superfluid.RealtimeBalance_prop
import qualified Money.Superfluid.System_unit

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "RealtimeBalance prop"  Money.Superfluid.RealtimeBalance_prop.tests
    , testGroup "System unit" Money.Superfluid.System_unit.tests
    ]
