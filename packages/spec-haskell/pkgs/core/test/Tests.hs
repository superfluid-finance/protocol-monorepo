import           Test.Hspec

import qualified Money.Systems.Superfluid.ConstantFlowAgreement_prop
import qualified Money.Systems.Superfluid.ConstantFlowDistributionAgreement_prop
import qualified Money.Systems.Superfluid.RealTimeBalance_prop

main :: IO ()
main = hspec $ do
    Money.Systems.Superfluid.RealTimeBalance_prop.tests
    Money.Systems.Superfluid.ConstantFlowAgreement_prop.tests
    Money.Systems.Superfluid.ConstantFlowDistributionAgreement_prop.tests
