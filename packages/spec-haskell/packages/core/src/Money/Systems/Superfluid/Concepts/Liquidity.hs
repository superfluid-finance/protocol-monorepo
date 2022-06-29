{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | = Liquidity Concept Refined for Superfluid Money
--
-- == Terminologies
--
-- In /Superfluid Money/, plain 'Liquidity' is also called untyped liquidity, while 'TypedLiquidity' is 'Liquidity' that
-- is tagged to be either 'UntappedLiquidity' or 'TappedLiquidity':
--
-- * Untapped liquidity is the type of liquidity that can be freely used by any sub-systems.
-- * Tapped liquidity is the type of liquidity that must only be used by a specific sub-system.
--
-- Here is how their relations look like:
--
-- @
--                 +--------------+
--                 |TaggedTypeable|
--                 +-------^------+
--                         |
--                +--------+--------+                                   +--------------+
--            +--->TypedLiquidityTag<-----+                       +----->TypedLiquidity<----+
--            |   +-----------------+     |                       |     +--------------+    |
--            |                           |                       |                         |
-- +----------+---------+     +-----------+---------+       +-----+-----------+   +---------+--------+
-- |UntappedLiquidityTag|     |(C)TappedLiquidityTag| ===>> |UntappedLiquidity|   |(C)TappedLiquidity|
-- +--------------------+     +-----------@---------+       +-----------------+   +---------@--------+
--                                        |                                                 |
--                            +-----------+---------+                             +---------+--------+
--                            |AnyTappedLiquidityTag|                             |AnyTappedLiquidity|
--                            +---------------------+                             +------------------+
-- @
-- [(ASCIIFlow Link)](https://asciiflow.com/#/share/eJyrVspLzE1VslIKLi1ILUrLKc1MUfDJLARSmSWVSjpKOYmVqUVA6eoYpYoYJStLcwOdGKVKIMvIEsQqSa0oAXJilBTQwKMpe0hCMTF5WIwISUxPT00JqSxITUzKScWpDDuatotYm5CMwpQl1SdwhMsWqoUSmm9BwZQCjztg0GENAeJdgm7io%2BkthJxDcXDhdSBZQYY1XvHHDmFH4JADWkK29zHCgUomURYjlFgPCY3QvJLEAvSEifClhrNmCFZ5W1tbOzsFbAbAXYapl%2B5x4DDAMYDPfjwFHQnJGn9yx6%2BAShFBCxsIut0xrxJ7wiTgIkx9Qz2klGqVagHvSCn9)
--
-- == Known Sub-systems
--
-- * Agreements (TBA, CFA, IDA, etc.)
-- * Atomic Composite Agreement (ACA)
-- * Buffer Based Solvency (BBS)
--
module Money.Systems.Superfluid.Concepts.Liquidity
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
    ) where

import           Data.Default                (Default (..))
import           Data.Type.TaggedTypeable    (TaggedTypeable (..))
import           Data.Typeable               (Proxy (..), typeRep)

import           Money.Concepts.Distribution (Liquidity)

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
newtype UntappedLiquidity lq = UntappedLiquidity lq
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral)

instance Liquidity lq => TypedLiquidity (UntappedLiquidity lq) lq where
    typeLiquidity = UntappedLiquidity
    untypeLiquidity (UntappedLiquidity liq) = liq
    isLiquidityOfType _ liqt1 = typeRep liqt1 == typeRep untappedLiquidityTag

instance Liquidity lq => Show (UntappedLiquidity lq) where
    show (UntappedLiquidity liq) = show liq ++ "@_"

-- | Tapped LiquidityTag Tag
--
class TypedLiquidityTag ltag => TappedLiquidityTag ltag

-- | Any Tapped Liquidity Tag Existential Type
data AnyTappedLiquidityTag where
    MkTappedLiquidityTag :: TappedLiquidityTag ltag => Proxy ltag -> AnyTappedLiquidityTag

-- | TappedLiquidity Type
--
-- Naming conventions for TypedLiquidity:
--  * Term name: tliq
--
newtype TappedLiquidity ltag lq = TappedLiquidity lq
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
newtype AnyTappedLiquidity lq = AnyTappedLiquidity (AnyTappedLiquidityTag, lq)

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
