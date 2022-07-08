{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( UntypedLiquidityVector (..)
    , TypedLiquidityVector (..)
    , RealtimeBalance (..)
    , RealtimeBalanceDerivingHelper (..)
    ) where

import           Data.Default

import           Money.Systems.Superfluid.Concepts.TypedValue
    ( AnyTappedValue (..)
    , Value
    , UntappedValue (..)
    )

-- | UntypedLiquidityVector type
--
data UntypedLiquidityVector v = UntypedLiquidityVector v [v]

-- | Create an untyped value vector from a list of value
mk_untyped_value_vector :: Value v => [v] -> UntypedLiquidityVector v
mk_untyped_value_vector (uval:xs) = UntypedLiquidityVector uval xs
mk_untyped_value_vector _         = error "Untapped value missing"

-- | TypedLiquidityVector type
--
data TypedLiquidityVector v = TypedLiquidityVector (UntappedValue v) [AnyTappedValue v]

-- | RealtimeBalance Type Class
--
-- Notional conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: *RTB *Balance
class (Value v, Num rtb, Default rtb) => RealtimeBalance rtb v | rtb -> v where
    valueVectorFromRTB :: rtb -> [v]
    valueVectorFromRTB b = let
        (TypedLiquidityVector (UntappedValue uval) avals) = typedLiquidityVectorFromRTB b
        auvals = map (\(AnyTappedValue aval) -> snd aval) avals
        in uval:auvals

    typedLiquidityVectorFromRTB :: rtb -> TypedLiquidityVector v

    valueToRTB :: v -> rtb

    typedLiquidityVectorToRTB :: TypedLiquidityVector v -> rtb

    untypedLiquidityVectorToRTB :: UntypedLiquidityVector v -> rtb

    untappedLiquidityFromRTB :: (Value v, RealtimeBalance rtb v) => rtb -> v
    untappedLiquidityFromRTB = get_untyped_value . mk_untyped_value_vector . valueVectorFromRTB
        where get_untyped_value (UntypedLiquidityVector uval _) = uval

    valueRequiredForRTB :: (Value v, RealtimeBalance rtb v) => rtb -> v
    valueRequiredForRTB = foldr (+) def . valueVectorFromRTB

    mormalizeRTBWith :: (v -> v) -> rtb -> rtb
    mormalizeRTBWith f = valueToRTB . f . valueRequiredForRTB

-- | RealtimeBalanceDerivingHelper Type
--
-- To use:
--   - enable DerivingVia language extension
--   - do @deriving Num via RTB.RealtimeBalanceDerivingHelper SimpleRealtimeBalance Wad@
--
newtype RealtimeBalanceDerivingHelper rtb v = RealtimeBalanceDerivingHelper rtb

-- | RealtimeBalance Num type class deriving helper
instance (Value v, RealtimeBalance rtb v) => Num (RealtimeBalanceDerivingHelper rtb v) where
    (+) (RealtimeBalanceDerivingHelper a) (RealtimeBalanceDerivingHelper b) = RealtimeBalanceDerivingHelper $
        untypedLiquidityVectorToRTB . mk_untyped_value_vector $
        zipWith (+) (valueVectorFromRTB a) (valueVectorFromRTB b)
    (*) (RealtimeBalanceDerivingHelper a) (RealtimeBalanceDerivingHelper b) = RealtimeBalanceDerivingHelper $
        valueToRTB $
        valueRequiredForRTB a * valueRequiredForRTB b
    fromInteger x = RealtimeBalanceDerivingHelper $
        valueToRTB . fromInteger $ x
    signum (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        mormalizeRTBWith signum x
    abs (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        mormalizeRTBWith abs x
    negate (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        mormalizeRTBWith negate x
