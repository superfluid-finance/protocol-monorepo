{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | = Value Concept Refined for Superfluid Money
--
-- == Terminologies
--
-- In /Superfluid Money/, plain 'Value' is also called untyped value, while 'TypedValue' is 'Value' that
-- is tagged to be either 'UntappedValue' or 'TappedValue':
--
-- * Untapped value is the type of value that can be freely used by any sub-systems.
-- * Tapped value is the type of value that must only be used by a specific sub-system.
--
-- Here is how their relations look like:
--
-- @
--                 +--------------+
--                 |TaggedTypeable|
--                 +-------^------+
--                         |
--                +--------+--------+                                   +--------------+
--            +--->TypedLiquidityTag<-----+                       +----->TypedValue<----+
--            |   +-----------------+     |                       |     +--------------+    |
--            |                           |                       |                         |
-- +----------+---------+     +-----------+---------+       +-----+-----------+   +---------+--------+
-- |UntappedLiquidityTag|     |(C)TappedValueTag| ===>> |UntappedValue|   |(C)TappedValue|
-- +--------------------+     +-----------@---------+       +-----------------+   +---------@--------+
--                                        |                                                 |
--                            +-----------+---------+                             +---------+--------+
--                            |AnyTappedValueTag|                             |AnyTappedValue|
--                            +---------------------+                             +------------------+
-- @
-- [(ASCIIFlow Link)](https://asciiflow.com/#/share/eJyrVspLzE1VslIKLi1ILUrLKc1MUfDJLARSmSWVSjpKOYmVqUVA6eoYpYoYJStLcwOdGKVKIMvIEsQqSa0oAXJilBTQwKMpe0hCMTF5WIwISUxPT00JqSxITUzKScWpDDuatotYm5CMwpQl1SdwhMsWqoUSmm9BwZQCjztg0GENAeJdgm7io%2BkthJxDcXDhdSBZQYY1XvHHDmFH4JADWkK29zHCgUomURYjlFgPCY3QvJLEAvSEifClhrNmCFZ5W1tbOzsFbAbAXYapl%2B5x4DDAMYDPfjwFHQnJGn9yx6%2BAShFBCxsIut0xrxJ7wiTgIkx9Qz2klGqVagHvSCn9)
--
-- == Known Sub-systems
--
-- * Agreements (ITA, CFA, IDA, etc.)
-- * Atomic Composite Agreement (ACA)
-- * Buffer Based Solvency (BBS)
--
module Money.Systems.Superfluid.Concepts.TypedValue
    -- Untyped Value
    ( Value
    -- Typed Value
    , TypedValue
    -- Untapped Value
    , untappedValueTag
    , UntappedValue (..)
    -- Tapped Value
    , TappedValueTag (..)
    , TappedValue (..)
    -- Any Tapped Value
    , AnyTappedValueTag (..)
    , AnyTappedValue (..)
    , mkAnyTappedLiquidity
    , fromAnyTappedLiquidity
    ) where

import           Data.Coerce                 (Coercible)
import           Data.Default                (Default (..))
import           Data.Typeable               (Proxy (..), Typeable, typeRep)

import           Money.Concepts.Distribution (Value)


-- | Typed value type class
--
-- Notional conventions:
--  * Type name: tv
class (Value v, Coercible tv v) => TypedValue tv v | tv -> v

-- | Untapped value type
--
-- Notional conventions:
--  * Term name: uval
newtype UntappedValue v = UntappedValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, Value)
    deriving stock (Functor)

instance Value v => TypedValue (UntappedValue v) v

-- | Create untapped value tag
untappedValueTag :: Proxy UntappedValue
untappedValueTag = Proxy @UntappedValue

-- | Tag for tapped value type class
--
-- Notional conventions for TypedValue:
--  * Type name: vtag
class Typeable vtag => TappedValueTag vtag where tappedValueTag :: (Proxy vtag) -> String

-- | Tapped value type
--
-- Notional conventions for TypedValue:
--  * Term name: tval
newtype TappedValue vtag v = TappedValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, Value)
    deriving stock (Functor)

instance (Value v, TappedValueTag vtag) => TypedValue (TappedValue vtag v) v

-- | Tag for any tapped value
--
-- Notional conventions for TypedValue:
--  * Term name: avtag
data AnyTappedValueTag = forall vtag. TappedValueTag vtag => MkTappedValueTag (Proxy vtag)

-- | Any tapped value Type
--
-- Notional conventions for TypedValue:
--  * Term name: aval
newtype AnyTappedValue v = AnyTappedValue (AnyTappedValueTag, v)

-- | Create any tapped value
mkAnyTappedLiquidity
    :: forall vtag v. (Value v, TappedValueTag vtag)
    => TappedValue vtag v -> AnyTappedValue v
mkAnyTappedLiquidity (TappedValue uval) = AnyTappedValue (MkTappedValueTag (Proxy @vtag), uval)

-- | Get value from any tapped value if value tag matches, or return default value
fromAnyTappedLiquidity :: (Value v, Typeable vtag) => AnyTappedValue v -> Proxy vtag -> v
fromAnyTappedLiquidity (AnyTappedValue (MkTappedValueTag tag1, uval)) tag2 =
    if typeRep tag1 == typeRep tag2 then uval else def
