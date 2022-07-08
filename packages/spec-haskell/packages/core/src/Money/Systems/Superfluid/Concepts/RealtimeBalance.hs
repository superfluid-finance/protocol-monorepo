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
mk_untyped_liquidity_vector :: Value v => [v] -> UntypedLiquidityVector v
mk_untyped_liquidity_vector (uval:xs) = UntypedLiquidityVector uval xs
mk_untyped_liquidity_vector _         = error "Untapped value missing"

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
    liquidityVectorFromRTB :: rtb -> [v]
    liquidityVectorFromRTB b = let
        (TypedLiquidityVector (UntappedValue uval) aliqs) = typedLiquidityVectorFromRTB b
        auliqs = map (\(AnyTappedValue aval) -> snd aval) aliqs
        in uval:auliqs

    typedLiquidityVectorFromRTB :: rtb -> TypedLiquidityVector v

    liquidityToRTB :: v -> rtb

    typedLiquidityVectorToRTB :: TypedLiquidityVector v -> rtb

    untypedLiquidityVectorToRTB :: UntypedLiquidityVector v -> rtb

    untappedLiquidityFromRTB :: (Value v, RealtimeBalance rtb v) => rtb -> v
    untappedLiquidityFromRTB = get_untyped_liquidity . mk_untyped_liquidity_vector . liquidityVectorFromRTB
        where get_untyped_liquidity (UntypedLiquidityVector uval _) = uval

    liquidityRequiredForRTB :: (Value v, RealtimeBalance rtb v) => rtb -> v
    liquidityRequiredForRTB = foldr (+) def . liquidityVectorFromRTB

    mormalizeRTBWith :: (v -> v) -> rtb -> rtb
    mormalizeRTBWith f = liquidityToRTB . f . liquidityRequiredForRTB

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
        untypedLiquidityVectorToRTB . mk_untyped_liquidity_vector $
        zipWith (+) (liquidityVectorFromRTB a) (liquidityVectorFromRTB b)
    (*) (RealtimeBalanceDerivingHelper a) (RealtimeBalanceDerivingHelper b) = RealtimeBalanceDerivingHelper $
        liquidityToRTB $
        liquidityRequiredForRTB a * liquidityRequiredForRTB b
    fromInteger x = RealtimeBalanceDerivingHelper $
        liquidityToRTB . fromInteger $ x
    signum (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        mormalizeRTBWith signum x
    abs (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        mormalizeRTBWith abs x
    negate (RealtimeBalanceDerivingHelper x) = RealtimeBalanceDerivingHelper $
        mormalizeRTBWith negate x
