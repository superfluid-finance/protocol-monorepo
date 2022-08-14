{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- HLINT ignore "Monoid law, left identity"  -}
{- HLINT ignore "Monoid law, right identity" -}

module Money.Systems.Superfluid.RealTimeBalance_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.CoreTypes
import           Money.Systems.Superfluid.TestTypes


-- * Monoid Laws

monoid_right_identity :: T_RealTimeBalance -> Bool
monoid_right_identity x = (x <> mempty) == x

monoid_left_identity :: T_RealTimeBalance -> Bool
monoid_left_identity x = (mempty <> x) == x

monoid_associativity :: T_RealTimeBalance -> T_RealTimeBalance -> T_RealTimeBalance -> Bool
monoid_associativity a b c = ((a <> b) <> c) == (a <> (b <> c))

-- * Additional RTB Laws

monoid_mappend_commutativity :: T_RealTimeBalance -> T_RealTimeBalance -> Bool
monoid_mappend_commutativity a b = (a <> b) == (b <> a)

rtb_identity_from_and_to_typed_values :: T_RealTimeBalance -> Bool
rtb_identity_from_and_to_typed_values x = (uncurry typedValuesToRTB . typedValuesFromRTB) x == x

rtb_conservation_of_net_value :: T_MVal -> Bool
rtb_conservation_of_net_value x = (netValueOfRTB . valueToRTB) x == x

tests = describe "RealTimeBalance properties" $ do
    it "monoid right identity"                 $ property monoid_right_identity
    it "monoid left identity"                  $ property monoid_left_identity
    it "monoid associativity"                  $ property monoid_associativity
    it "monoid mappend commutativity"          $ property monoid_mappend_commutativity
    it "RTB identity from and to typed values" $ property rtb_identity_from_and_to_typed_values
    it "RTB conservation of net value"         $ property rtb_conservation_of_net_value
