{-# OPTIONS_GHC -Wno-missing-signatures #-}
{- HLINT ignore "Monoid law, left identity"  -}
{- HLINT ignore "Monoid law, right identity" -}

module Money.Systems.Superfluid.RealTimeBalance_prop (tests) where

import           Data.Proxy
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

rtb_mappend_commutativity :: T_RealTimeBalance -> T_RealTimeBalance -> Bool
rtb_mappend_commutativity = rtb_prop_mappend_commutativity

rtb_identity_from_and_to_typed_values :: T_RealTimeBalance -> Bool
rtb_identity_from_and_to_typed_values = rtb_prop_identity_from_and_to_typed_values

rtb_conservation_of_net_value :: T_MVal -> Bool
rtb_conservation_of_net_value = rtb_prop_conservation_of_net_value (Proxy @T_RealTimeBalanceF)

rtb_untapped_value_to_rtb :: T_MVal -> Bool
rtb_untapped_value_to_rtb = rtb_prop_untapped_value_to_rtb (Proxy @T_RealTimeBalanceF)

tests = describe "RealTimeBalance properties" $ do
    it "monoid right identity"                 $ property monoid_right_identity
    it "monoid left identity"                  $ property monoid_left_identity
    it "monoid associativity"                  $ property monoid_associativity
    it "RTB mappend commutativity"             $ property rtb_mappend_commutativity
    it "RTB identity from and to typed values" $ property rtb_identity_from_and_to_typed_values
    it "RTB conservation of net value"         $ property rtb_conservation_of_net_value
    it "RTB untapped value to rtb"             $ property rtb_untapped_value_to_rtb
