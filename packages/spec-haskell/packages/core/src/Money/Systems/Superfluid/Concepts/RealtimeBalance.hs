{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( UntypedValueVector (..)
    , TypedValueVector (..)
    , RealtimeBalance (..)
    , RealtimeBalanceDerivingHelper (..)
    ) where

import           Data.Default

import           Money.Systems.Superfluid.Concepts.TypedValue (AnyTappedValue (..), UntappedValue (..), Value)

-- | UntypedValueVector type
--
data UntypedValueVector v = UntypedValueVector v [v]

-- | Create an untyped value vector from a list of value
mk_untyped_value_vector :: Value v => [v] -> UntypedValueVector v
mk_untyped_value_vector (uval:xs) = UntypedValueVector uval xs
mk_untyped_value_vector _         = error "Untapped value missing"

-- | TypedValueVector type
--
data TypedValueVector v = TypedValueVector (UntappedValue v) [AnyTappedValue v]

-- | RealtimeBalance Type Class
--
-- Notional conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: *RTB *Balance
class (Value v, Num rtb, Default rtb) => RealtimeBalance rtb v | rtb -> v where
    valueVectorFromRTB :: rtb -> [v]
    valueVectorFromRTB b = let
        (TypedValueVector (UntappedValue uval) avals) = typedValueVectorFromRTB b
        auvals = map (\(AnyTappedValue aval) -> snd aval) avals
        in uval:auvals

    typedValueVectorFromRTB :: rtb -> TypedValueVector v

    valueToRTB :: v -> rtb

    typedValueVectorToRTB :: TypedValueVector v -> rtb

    untypedValueVectorToRTB :: UntypedValueVector v -> rtb

    untappedValueFromRTB :: (Value v, RealtimeBalance rtb v) => rtb -> v
    untappedValueFromRTB = get_untyped_value . mk_untyped_value_vector . valueVectorFromRTB
        where get_untyped_value (UntypedValueVector uval _) = uval

    valueRequiredForRTB :: (Value v, RealtimeBalance rtb v) => rtb -> v
    valueRequiredForRTB = foldr (+) def . valueVectorFromRTB

    normalizeRTBWith :: (v -> v) -> rtb -> rtb
    normalizeRTBWith f = valueToRTB . f . valueRequiredForRTB

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
        untypedValueVectorToRTB . mk_untyped_value_vector $
        zipWith (+) (valueVectorFromRTB a) (valueVectorFromRTB b)
    (*) (RealtimeBalanceDerivingHelper a) (RealtimeBalanceDerivingHelper b) = RealtimeBalanceDerivingHelper $
        valueToRTB $
        valueRequiredForRTB a * valueRequiredForRTB b
    fromInteger x = RealtimeBalanceDerivingHelper $
        valueToRTB . fromInteger $ x
    signum (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        normalizeRTBWith signum x
    abs (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        normalizeRTBWith abs x
    negate (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        normalizeRTBWith negate x
