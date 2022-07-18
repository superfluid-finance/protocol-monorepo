{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module Money.Systems.Superfluid.RealTimeBalance_prop (tests) where

import           Data.Foldable                                   (toList)
import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.Instances.Simple.Types


-- QuickCheck helpers
instance Arbitrary SimpleRealTimeBalance where
    arbitrary = do
        u  <- arbitrary
        m  <- arbitrary
        d  <- arbitrary
        od <- arbitrary
        return SimpleRealTimeBalanceF
            { untappedValue    = Wad u
            , mintedValue      = Wad m
            , depositValue     = Wad d
            , owedDepositValue = Wad od
            }

sameAs :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
sameAs a b = and $ zipWith (==) (toList a) (toList b)

-- Conventional properties of Num:
prop_plus_has_associativity :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
prop_plus_has_associativity a b c = ((a + b) + c) `sameAs` (a + (b + c))

prop_plus_has_commutativity :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
prop_plus_has_commutativity a b = (a + b) `sameAs` (b + a)

prop_from_integer_zero_is_add_id :: SimpleRealTimeBalance -> Bool
prop_from_integer_zero_is_add_id x = (x + addId) `sameAs` x where addId = 0

prop_mul_has_associativity :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
prop_mul_has_associativity a b c = ((a * b) * c) `sameAs` (a * (b * c))

prop_mul_add_distributivity :: SimpleRealTimeBalance -> SimpleRealTimeBalance -> SimpleRealTimeBalance -> Bool
prop_mul_add_distributivity a b c =
        (a * (b + c)) `sameAs` ((a * b) + (a * c)) &&
        ((b + c) * a) `sameAs` ((b * a) + (c * a))

-- Superfluid specific properties of Num
-- Conventionally:
--   1. abs x * signum x == x
--   2. negate gives the additive inverse
--      x + negate x = fromInteger 0
--   3. fromInteger 1 is the multiplicative identity
--      x * fromInteger 1 = x and fromInteger 1 * x = x
-- Superfluid defines the semantic of normalizing RTB for (*), negate & abs, hence:
--   1. signum x * abs x = normalized x
--   2. (signum x * abs x) + negate x = fromInteger 0
--   3. x * fromInteger 1 = normalized x and fromInteger 1 * x = normalized x

prop_abs_signum_eq_normalize_rtb_with_id :: SimpleRealTimeBalance -> Bool
prop_abs_signum_eq_normalize_rtb_with_id x = (signum x * abs x) `sameAs` normalizeRTBWith id x

prop_abs_signum_plus_inv_eq_zero :: SimpleRealTimeBalance -> Bool
prop_abs_signum_plus_inv_eq_zero x = (signum x * abs x - x) `sameAs` 0

prop_from_integer_one_is_mul_id :: SimpleRealTimeBalance -> Bool
prop_from_integer_one_is_mul_id x = let mulId = 1 in
    (x * mulId) `sameAs` normalizeRTBWith id x &&
    (mulId * x) `sameAs` normalizeRTBWith id x

tests = describe "RealTimeBalance properties" $ do
    it "(+) has associativity" $ property prop_plus_has_associativity
    it "(+) has commutativity" $ property prop_plus_has_commutativity
    it "(fromInteger 0) is the additive identity" $ property prop_from_integer_zero_is_add_id
    it "(*) has associativity" $ property prop_mul_has_associativity
    it "distributivity of (*) with respect to (+)" $ property prop_mul_add_distributivity

    it "(abs x * signum x) equals x" $ property prop_abs_signum_eq_normalize_rtb_with_id
    it "(abs x * signum x) plus -x equals 0" $ property prop_abs_signum_plus_inv_eq_zero
    it "(fromInteger 1) is the multiplicative identity" $ property prop_from_integer_one_is_mul_id
