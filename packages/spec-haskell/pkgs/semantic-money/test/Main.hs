import           Test.Hspec

import qualified Money.Theory.SemanticMoney_prop

main :: IO ()
main = hspec $ do
    Money.Theory.SemanticMoney_prop.tests
