{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module Money.Superfluid.RealtimeBalance_prop (tests) where

import           Test.Framework.Providers.QuickCheck2              (testProperty)
import           Test.QuickCheck

import           Money.Superfluid.Instances.Simple.SuperfluidTypes
    ( RealtimeBalance (..)
    , SimpleRealtimeBalance (..)
    , Wad (..)
    )


-- QuickCheck helpers
instance Arbitrary SimpleRealtimeBalance where
    arbitrary = do
        uliq <- arbitrary
        mliq <- arbitrary
        d <- arbitrary
        od <- arbitrary
        return SimpleRealtimeBalance
            { untappedLiquidityVal = Wad uliq
            , mintedVal = Wad mliq
            , depositVal = Wad d
            , owedDepositVal = Wad od
            }

sameAs :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
sameAs a b = and $ zipWith (==) (liquidityVectorFromRTB a) (liquidityVectorFromRTB b)

-- Conventional properties of Num:
prop_plusHasAssociativity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_plusHasAssociativity a b c = ((a + b) + c) `sameAs` (a + (b + c))

prop_plusHasCommutativity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_plusHasCommutativity a b = (a + b) `sameAs` (b + a)

prop_fromIntegerZeroIsAddId :: SimpleRealtimeBalance -> Bool
prop_fromIntegerZeroIsAddId x = (x + addId) `sameAs` x where addId = 0

prop_mulHasAssociativity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_mulHasAssociativity a b c = ((a * b) * c) `sameAs` (a * (b * c))

prop_mulAddDistributivity :: SimpleRealtimeBalance -> SimpleRealtimeBalance -> SimpleRealtimeBalance -> Bool
prop_mulAddDistributivity a b c =
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
--   1. abs x * signum x = normalized x
--   2. (signum x * abs x) + negate x = fromInteger 0
--   3. x * fromInteger 1 = normalized x and fromInteger 1 * x = normalized x
prop_requiredLiquidityRTB :: SimpleRealtimeBalance -> Bool
prop_requiredLiquidityRTB x = (abs x * signum x) `sameAs` mormalizeRTBWith id x

prop_requiredLiquidityRTBInv :: SimpleRealtimeBalance -> Bool
prop_requiredLiquidityRTBInv x = ((signum x * abs x) - x) `sameAs` 0

prop_fromIntegerOneIsMulId :: SimpleRealtimeBalance -> Bool
prop_fromIntegerOneIsMulId x = let mulId = 1 in
    (x * mulId) `sameAs` mormalizeRTBWith id x &&
    (mulId * x) `sameAs` mormalizeRTBWith id x

tests =
    [ testProperty "(+) has associativity" prop_plusHasAssociativity
    , testProperty "(+) has commutativity" prop_plusHasCommutativity
    , testProperty "(fromInteger 0) is the additive identity" prop_fromIntegerZeroIsAddId
    , testProperty "(abs x * signum x) equals normalized x" prop_requiredLiquidityRTB
    , testProperty "(signum x * abs x) - x equals zero" prop_requiredLiquidityRTBInv
    , testProperty "(*) has associativity" prop_mulHasAssociativity
    , testProperty "(fromInteger 1) is the multiplicative identity normalized" prop_fromIntegerOneIsMulId
    , testProperty "distributivity of (*) with respect to (+)" prop_mulAddDistributivity
    ]
