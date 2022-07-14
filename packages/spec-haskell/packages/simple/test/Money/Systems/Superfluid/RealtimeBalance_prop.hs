{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module Money.Systems.Superfluid.RealtimeBalance_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.Instances.Simple.Types
    ( RealtimeBalance (..)
    , SimpleRealtimeBalance (..)
    , Wad (..)
    )


-- QuickCheck helpers
instance Arbitrary SimpleRealtimeBalance where
    arbitrary = do
        uval <- arbitrary
        mval <- arbitrary
        d <- arbitrary
        od <- arbitrary
        return SimpleRealtimeBalance
            { untappedValueVal = Wad uval
            , mintedVal = Wad mval
            , depositVal = Wad d
            , owedDepositVal = Wad od
            }

sameAs :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
sameAs a b = and $ zipWith (==) (valueVectorFromRTB a) (valueVectorFromRTB b)

-- Conventional properties of Num:
prop_plus_has_associativity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_plus_has_associativity a b c = ((a + b) + c) `sameAs` (a + (b + c))

prop_plus_has_commutativity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_plus_has_commutativity a b = (a + b) `sameAs` (b + a)

prop_from_integer_zero_is_add_id :: SimpleRealtimeBalance -> Bool
prop_from_integer_zero_is_add_id x = (x + addId) `sameAs` x where addId = 0

prop_mul_has_associativity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_mul_has_associativity a b c = ((a * b) * c) `sameAs` (a * (b * c))

prop_mul_add_distributivity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
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

prop_abssignum_eq_normalize_rtb_with_id :: SimpleRealtimeBalance -> Bool
prop_abssignum_eq_normalize_rtb_with_id x = (signum x * abs x) `sameAs` normalizeRTBWith id x

prop_abssignum_plus_inv_eq_zero :: SimpleRealtimeBalance -> Bool
prop_abssignum_plus_inv_eq_zero x = (signum x * abs x - x) `sameAs` 0

prop_from_integer_one_is_mul_id :: SimpleRealtimeBalance -> Bool
prop_from_integer_one_is_mul_id x = let mulId = 1 in
    (x * mulId) `sameAs` normalizeRTBWith id x &&
    (mulId * x) `sameAs` normalizeRTBWith id x

tests = describe "RealtimeBalance properties" $ do
    it "(+) has associativity" $ property prop_plus_has_associativity
    it "(+) has commutativity" $ property prop_plus_has_commutativity
    it "(fromInteger 0) is the additive identity" $ property prop_from_integer_zero_is_add_id
    it "(*) has associativity" $ property prop_mul_has_associativity
    it "distributivity of (*) with respect to (+)" $ property prop_mul_add_distributivity

    it "(abs x * signum x) equals normalized x" $ property prop_abssignum_eq_normalize_rtb_with_id
    it "(abs x * signum x) plus -x equals 0" $ property prop_abssignum_plus_inv_eq_zero
    it "(fromInteger 1) is the multiplicative identity normalized" $ property prop_from_integer_one_is_mul_id
