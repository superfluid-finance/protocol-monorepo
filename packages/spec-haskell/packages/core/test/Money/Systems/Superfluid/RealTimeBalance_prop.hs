{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
{- HLINT ignore "Monoid law, left identity"  -}
{- HLINT ignore "Monoid law, right identity" -}

module Money.Systems.Superfluid.RealTimeBalance_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.Concepts
import           Money.Systems.Superfluid.TestTypes


-- * Helpers

sameAs :: TestRealTimeBalance -> TestRealTimeBalance -> Bool
sameAs a b = and $ (==) <$> a <*> b

-- * Monoid Laws

monoid_right_identity :: TestRealTimeBalance -> Bool
monoid_right_identity x = (x <> mempty) `sameAs` x

monoid_left_identity :: TestRealTimeBalance -> Bool
monoid_left_identity x = (mempty <> x) `sameAs` x

monoid_associativity :: TestRealTimeBalance -> TestRealTimeBalance -> TestRealTimeBalance -> Bool
monoid_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

-- * Additional RTB Laws

monoid_mappend_commutativity :: TestRealTimeBalance -> TestRealTimeBalance -> Bool
monoid_mappend_commutativity a b = (a <> b) `sameAs` (b <> a)

rtb_identity_from_and_to_typed_values :: TestRealTimeBalance -> Bool
rtb_identity_from_and_to_typed_values x = (uncurry typedValuesToRTB . typedValuesFromRTB) x `sameAs` x

rtb_conservation_of_net_value :: TestMVal -> Bool
rtb_conservation_of_net_value x = (netValueOfRTB . valueToRTB) x == x

tests = describe "RealTimeBalance properties" $ do
    it "monoid right identity"                 $ property monoid_right_identity
    it "monoid left identity"                  $ property monoid_left_identity
    it "monoid associativity"                  $ property monoid_associativity
    it "monoid mappend commutativity"          $ property monoid_mappend_commutativity
    it "RTB identity from and to typed values" $ property rtb_identity_from_and_to_typed_values
    it "RTB conservation of net value"         $ property rtb_conservation_of_net_value
