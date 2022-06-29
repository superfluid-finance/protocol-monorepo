{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( UntypedLiquidityVector (..)
    , TypedLiquidityVector (..)
    , RealtimeBalance (..)
    , RealtimeBalanceDerivingHelper (..)
    ) where

import           Data.Default

import           Money.Systems.Superfluid.Concepts.Liquidity (AnyTappedLiquidity, Liquidity, UntappedLiquidity)

-- | UntypedLiquidityVector type
--
data UntypedLiquidityVector lq = UntypedLiquidityVector lq [lq]

-- | Create an untyped liquidity vector from a list of liquidity
mk_untyped_liquidity_vector :: Liquidity lq => [lq] -> UntypedLiquidityVector lq
mk_untyped_liquidity_vector (uliq:xs) = UntypedLiquidityVector uliq xs
mk_untyped_liquidity_vector _         = error "Untapped liquidity missing"

-- | TypedLiquidityVector type
--
data TypedLiquidityVector lq = TypedLiquidityVector (UntappedLiquidity lq) [AnyTappedLiquidity lq]

-- | RealtimeBalance Type Class
--
-- Naming conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: *RTB *Balance
class (Liquidity lq, Num rtb, Default rtb, Show rtb) => RealtimeBalance rtb lq | rtb -> lq where
    liquidityVectorFromRTB :: rtb -> [lq]

    typedLiquidityVectorFromRTB :: rtb -> TypedLiquidityVector lq

    liquidityToRTB :: lq -> rtb

    typedLiquidityVectorToRTB :: TypedLiquidityVector lq -> rtb

    untypedLiquidityVectorToRTB :: UntypedLiquidityVector lq -> rtb

    untappedLiquidityFromRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
    untappedLiquidityFromRTB = get_untyped_liquidity . mk_untyped_liquidity_vector . liquidityVectorFromRTB
        where get_untyped_liquidity (UntypedLiquidityVector uliq _) = uliq

    liquidityRequiredForRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
    liquidityRequiredForRTB = foldr (+) def . liquidityVectorFromRTB

    mormalizeRTBWith :: (lq -> lq) -> rtb -> rtb
    mormalizeRTBWith f = liquidityToRTB . f . liquidityRequiredForRTB

-- | RealtimeBalanceDerivingHelper Type
--
-- To use:
--   - enable DerivingVia language extension
--   - do @deriving Num via RTB.RealtimeBalanceDerivingHelper SimpleRealtimeBalance Wad@
--
newtype RealtimeBalanceDerivingHelper rtb lq = RealtimeBalanceDerivingHelper rtb

instance (Liquidity lq, RealtimeBalance rtb lq) => Num (RealtimeBalanceDerivingHelper rtb lq) where
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

instance (Liquidity lq, RealtimeBalance rtb lq) => Show (RealtimeBalanceDerivingHelper rtb lq) where
    show (RealtimeBalanceDerivingHelper rtb) =
        (show . liquidityRequiredForRTB $ rtb) ++ " " ++
        (showDetail . typedLiquidityVectorFromRTB $ rtb)
        where
        showDetail (TypedLiquidityVector uliq tvec) = "( "
            ++ show uliq
            -- This is a version that ignores any zero liquidity scalar:
            -- ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . untypeLiquidity )) $ tvec)
            ++ foldl ((++) . (++ ", ")) "" (map show tvec)
            ++ " )"
