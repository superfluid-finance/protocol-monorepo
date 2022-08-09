{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- HLINT ignore "Monoid law, left identity"  -}
{- HLINT ignore "Monoid law, right identity" -}

module Money.Systems.Superfluid.ConstantFlowAgreement_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.Concepts
import           Money.Systems.Superfluid.TestTypes

-- * Monoid Laws

monoid_right_identity :: TestCFAMonetaryUnitData -> Bool
monoid_right_identity x = (x <> mempty) == x

monoid_left_identity :: TestCFAMonetaryUnitData -> Bool
monoid_left_identity x = (mempty <> x) == x

monoid_associativity :: TestCFAMonetaryUnitData -> TestCFAMonetaryUnitData -> TestCFAMonetaryUnitData -> Bool
monoid_associativity a b c = ((a <> b) <> c) == (a <> (b <> c))

-- * Additional RTB Laws

-- rtb_identity_from_and_to_typed_values :: TestRealTimeBalance -> Bool
-- rtb_identity_from_and_to_typed_values x = (uncurry typedValuesToRTB . typedValuesFromRTB) x `sameAs` x

-- rtb_conservation_of_net_value :: TestMVal -> Bool
-- rtb_conservation_of_net_value x = (netValueOfRTB . valueToRTB) x == x

tests = describe "ConstantFlowAgreement properties" $ do
    it "monoid right identity"                 $ property monoid_right_identity
    it "monoid left identity"                  $ property monoid_left_identity
    it "monoid associativity"                  $ property monoid_associativity
    -- it "RTB identity from and to typed values" $ property rtb_identity_from_and_to_typed_values
    -- it "RTB conservation of net value"         $ property rtb_conservation_of_net_value
