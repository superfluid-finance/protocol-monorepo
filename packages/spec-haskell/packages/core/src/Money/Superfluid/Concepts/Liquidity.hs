{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Liquidity Concept
--
-- Terminology:
--  * Untyped liquidity: Liquidity with unspecified types
--  * Untapped liquidity: Type of liquidity that can be freely used by any sub-systems (without tag)
--  * Tapped liquidity: Type of liquidity that must be specific to a sub-system (with tag)
--  * Typed liquidity: Liquidity is either untapped or tapped
--
-- Known sub-systems to be introduced later:
--  * Agreements (TBA, CFA, IDA, etc.)
--  * Atomic Composite Agreement (ACA)
--  * Buffer Based Solvency (BBS)
--
module Money.Superfluid.Concepts.Liquidity
    -- Untyped Liquidity
    ( Liquidity
    -- Typed Liquidity
    , TypedLiquidityTag
    , TypedLiquidity (..)
    -- Untapped Liquidity
    , UntappedLiquidityTag
    , untappedLiquidityTag
    , UntappedLiquidity (..)
    , untapLiquidity
    -- Tapped Liquidity
    , TappedLiquidityTag
    , TappedLiquidity (..)
    -- Any Tapped Liquidity
    , AnyTappedLiquidityTag (..)
    , AnyTappedLiquidity (..)
    , mkAnyTappedLiquidity
    -- Timestamp
    , Timestamp
    ) where

import           Data.Default
import           Data.Typeable

import           Money.Superfluid.Concepts.TaggedTypeable


-- | (Untyped) Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * SuperfluidTypes type indexer: SFT_LQ
class (Default lq, Integral lq, Show lq) => Liquidity lq

-- | TypedLiquidityTag Type Class
--
-- Naming conventions:
--  * Type name: tag
--
class TaggedTypeable ltag => TypedLiquidityTag ltag

-- | TypedLiquidity Type Class
--
-- Naming conventions:
--  * Type name: tlq
--
class (Show tlq, Liquidity lq) => TypedLiquidity tlq lq | tlq -> lq where
    typeLiquidity :: lq -> tlq
    untypeLiquidity :: tlq -> lq
    isLiquidityOfType :: TypedLiquidityTag ltag => tlq -> Proxy ltag -> Bool
    untypeLiquidityOfType :: TypedLiquidityTag ltag => tlq -> Proxy ltag -> lq
    untypeLiquidityOfType tliq tag = if tliq `isLiquidityOfType` tag then untypeLiquidity tliq else def
    mapLiquidity :: (lq -> lq) -> tlq -> tlq
    mapLiquidity f = typeLiquidity . f . untypeLiquidity

-- | UntappedLiquidityTag
--
data UntappedLiquidityTag

instance TaggedTypeable UntappedLiquidityTag where tagFromProxy _ = "_"
instance TypedLiquidityTag UntappedLiquidityTag

untappedLiquidityTag :: Proxy UntappedLiquidityTag
untappedLiquidityTag = Proxy @UntappedLiquidityTag

-- | UntappedLiquidity Type
--
-- Naming conventions:
--  * Term name: uliq
--
newtype Liquidity lq => UntappedLiquidity lq = UntappedLiquidity lq
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral)

instance Liquidity lq => TypedLiquidity (UntappedLiquidity lq) lq where
    typeLiquidity = UntappedLiquidity
    untypeLiquidity (UntappedLiquidity liq) = liq
    isLiquidityOfType _ liqt1 = typeRep liqt1 == typeRep untappedLiquidityTag

instance Liquidity lq => Show (UntappedLiquidity lq) where
    show (UntappedLiquidity liq) = show liq ++ "@_"

-- | TappedLiquidityTag type class and its Any Type
--
class TypedLiquidityTag ltag => TappedLiquidityTag ltag

data AnyTappedLiquidityTag where
    MkTappedLiquidityTag :: TappedLiquidityTag ltag => Proxy ltag -> AnyTappedLiquidityTag

-- | TappedLiquidity Type
--
-- Naming conventions for TypedLiquidity:
--  * Term name: tliq
--
newtype (TappedLiquidityTag ltag, Liquidity lq) => TappedLiquidity ltag lq = TappedLiquidity lq
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral)

untapLiquidity :: (TappedLiquidityTag ltag, Liquidity lq) => TappedLiquidity ltag lq -> lq
untapLiquidity (TappedLiquidity liq) = liq

instance (TappedLiquidityTag ltag, Liquidity lq) => TypedLiquidity (TappedLiquidity ltag lq) lq where
    typeLiquidity = TappedLiquidity
    untypeLiquidity (TappedLiquidity liq) = liq
    isLiquidityOfType _ tag2 = typeRep (Proxy @ltag) == typeRep tag2

instance (TappedLiquidityTag ltag, Liquidity lq) => Show (TappedLiquidity ltag lq) where
    show (TappedLiquidity liq) = show liq ++ "@" ++ tagFromProxy (Proxy @ltag)

-- | AnyTappedLiquidity Type
--
-- Naming conventions for TypedLiquidity:
--  * Term name: tliq
--
newtype Liquidity lq => AnyTappedLiquidity lq = AnyTappedLiquidity (AnyTappedLiquidityTag, lq)

mkAnyTappedLiquidity
    :: forall ltag lq. (TappedLiquidityTag ltag, Liquidity lq)
    => TappedLiquidity ltag lq -> AnyTappedLiquidity lq
mkAnyTappedLiquidity (TappedLiquidity liq) = AnyTappedLiquidity (MkTappedLiquidityTag (Proxy @ltag), liq)

instance Liquidity lq => TypedLiquidity (AnyTappedLiquidity lq) lq where
    typeLiquidity _ = error "No TypedLiquidity information for AnyTappedLiquidity"
    untypeLiquidity (AnyTappedLiquidity (_, liq)) = liq
    isLiquidityOfType (AnyTappedLiquidity (MkTappedLiquidityTag tag1, _)) tag2 = typeRep tag1 == typeRep tag2

instance Liquidity lq => Show (AnyTappedLiquidity lq) where
    show (AnyTappedLiquidity (MkTappedLiquidityTag tag, liq)) = show liq ++ "@" ++ tagFromProxy tag

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * SuperfluidTypes type indexer: SFT_TS
class (Default ts, Integral ts, Show ts) => Timestamp ts
