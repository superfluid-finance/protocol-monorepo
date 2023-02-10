import           Test.Hspec

import qualified Money.Systems.Superfluid.ConstantFlowAgreement_test
import qualified Money.Systems.Superfluid.ConstantFlowDistributionAgreement_test
import qualified Money.Systems.Superfluid.DecayingFlowAgreement_test
import qualified Money.Systems.Superfluid.InstantDistributionAgreement_test

main :: IO ()
main = hspec $ do
    Money.Systems.Superfluid.ConstantFlowAgreement_test.tests
    Money.Systems.Superfluid.DecayingFlowAgreement_test.tests
    Money.Systems.Superfluid.InstantDistributionAgreement_test.tests
    Money.Systems.Superfluid.ConstantFlowDistributionAgreement_test.tests
