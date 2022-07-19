import           Test.Hspec

import qualified Money.Systems.Superfluid.ConstantFlowAgreement_test
import qualified Money.Systems.Superfluid.DecayingFlowAgreement_test
import qualified Money.Systems.Superfluid.RealTimeBalance_prop

main :: IO ()
main = hspec $ do
    Money.Systems.Superfluid.RealTimeBalance_prop.tests
    Money.Systems.Superfluid.ConstantFlowAgreement_test.tests
    Money.Systems.Superfluid.DecayingFlowAgreement_test.tests
