{-# OPTIONS_GHC -Wno-unused-imports #-}
import           Test.Hspec

import           Data.Default
import           Money.Theory.SemanticMoney
import qualified Money.Theory.SemanticMoney_prop
import           Money.Theory.TestMonetaryTypes

main :: IO ()
main = hspec $ do
    Money.Theory.SemanticMoney_prop.tests
