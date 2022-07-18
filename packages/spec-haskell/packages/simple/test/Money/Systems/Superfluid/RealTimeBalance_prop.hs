{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module Money.Systems.Superfluid.RealTimeBalance_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.Instances.Simple.Types


-- * QuickCheck helpers

instance Arbitrary Wad where
    arbitrary = arbitrary >>= return . Wad

instance Arbitrary SimpleRealTimeBalance where
    arbitrary = do
        u  <- arbitrary
        m  <- arbitrary
        d  <- arbitrary
        return SimpleRealTimeBalanceF
            { untappedValue    = Wad u
            , mintedValue      = Wad m
            , depositValue     = Wad d
            }

sameAs :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
sameAs a b = and $ (==) <$> a <*> b

-- * Monoid Laws

monoid_right_identity :: SimpleRealTimeBalance -> Bool
monoid_right_identity x = (x <> mempty) `sameAs` x

monoid_left_identity :: SimpleRealTimeBalance -> Bool
monoid_left_identity x = (mempty <> x) `sameAs` x

monoid_associativity :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
monoid_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

-- * Additional RTB Laws

monoid_mappend_commutativity :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
monoid_mappend_commutativity a b = (a <> b) `sameAs` (b <> a)

rtb_identity_from_and_to_typed_values :: SimpleRealTimeBalance -> Bool
rtb_identity_from_and_to_typed_values x = ((uncurry typedValuesToRTB) . typedValuesFromRTB) x `sameAs` x

rtb_conservation_of_net_value :: Wad -> Bool
rtb_conservation_of_net_value x = (netValueOfRTB . valueToRTB) x == x

tests = describe "RealTimeBalance properties" $ do
    it "monoid right identity"                 $ property monoid_right_identity
    it "monoid left identity"                  $ property monoid_left_identity
    it "monoid associativity"                  $ property monoid_associativity
    it "monoid mappend commutativity"          $ property monoid_mappend_commutativity
    it "RTB identity from and to typed values" $ property rtb_identity_from_and_to_typed_values
    it "RTB conservation of net value"         $ property rtb_conservation_of_net_value
