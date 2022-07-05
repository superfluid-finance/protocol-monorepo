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
    , TypedLiquidity
    -- Untapped Liquidity
    , untappedLiquidityTag
    , UntappedLiquidity (..)
    -- Tapped Liquidity
    , TappedLiquidityTag
    , TappedLiquidity (..)
    -- Any Tapped Liquidity
    , AnyTappedLiquidityTag (..)
    , AnyTappedLiquidity (..)
    , mkAnyTappedLiquidity
    , fromAnyTappedLiquidity
    ) where

import           Data.Coerce                 (Coercible)
import           Data.Default                (Default (..))
import           Data.Type.TaggedTypeable    (TaggedTypeable (..))
import           Data.Typeable               (Proxy (..), typeRep)

import           Money.Concepts.Distribution (Liquidity)


-- | Typed liquidity type class
--
-- Notional conventions:
--  * Type name: tlq
class (Liquidity lq, Coercible tlq lq) => TypedLiquidity tlq lq | tlq -> lq

-- | Untapped liquidity type
--
-- Notional conventions:
--  * Term name: uliq
newtype UntappedLiquidity lq = UntappedLiquidity lq
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, Liquidity)
    deriving stock (Functor)

instance Liquidity lq => TypedLiquidity (UntappedLiquidity lq) lq

-- | Create untapped liquidity tag
untappedLiquidityTag :: Proxy UntappedLiquidity
untappedLiquidityTag = Proxy @UntappedLiquidity

-- | Tag for tapped liquidity type class
--
-- Notional conventions for TypedLiquidity:
--  * Type name: ltag
class TaggedTypeable ltag => TappedLiquidityTag ltag

-- | Tapped liquidity type
--
-- Notional conventions for TypedLiquidity:
--  * Term name: tliq
newtype TappedLiquidity ltag lq = TappedLiquidity lq
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, Liquidity)
    deriving stock (Functor)

instance (Liquidity lq, TappedLiquidityTag ltag) => TypedLiquidity (TappedLiquidity ltag lq) lq

-- | Tag for any tapped liquidity
data AnyTappedLiquidityTag = forall ltag. TappedLiquidityTag ltag => MkTappedLiquidityTag (Proxy ltag)

-- | Any tapped liquidity Type
--
-- Notional conventions for TypedLiquidity:
--  * Term name: aliq
newtype AnyTappedLiquidity lq = AnyTappedLiquidity (AnyTappedLiquidityTag, lq)

-- | Create any tapped liquidity
mkAnyTappedLiquidity
    :: forall ltag lq. (Liquidity lq, TappedLiquidityTag ltag)
    => TappedLiquidity ltag lq -> AnyTappedLiquidity lq
mkAnyTappedLiquidity (TappedLiquidity uliq) = AnyTappedLiquidity (MkTappedLiquidityTag (Proxy @ltag), uliq)

-- | Get liquidity from any tapped liquidity if liquidity tag matches, or return default value
fromAnyTappedLiquidity :: (Liquidity lq, TaggedTypeable ltag) => AnyTappedLiquidity lq -> Proxy ltag -> lq
fromAnyTappedLiquidity (AnyTappedLiquidity (MkTappedLiquidityTag tag1, uliq)) tag2 =
    if typeRep tag1 == typeRep tag2 then uliq else def
