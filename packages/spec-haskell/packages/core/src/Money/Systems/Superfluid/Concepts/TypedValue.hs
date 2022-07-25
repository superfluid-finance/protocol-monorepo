{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | = Value Concept Refined for Superfluid Money
--
-- == Terminologies
--
-- In /Superfluid Money Concepts/, plain 'Value' is also called untyped value, while 'TypedValue' is 'Value' that is
-- tagged to be either 'UntappedValue' or 'TappedValue':
--
-- * Untapped value is a type of value that can be freely used by any sub systems.
--
-- * Tapped value is a type of value that must only be used by its designated sub system.
--
-- Here is how their relations look like:
--
-- @
-- TODO
-- @
-- [(ASCIIFlow Link)](https://asciiflow.com/#/share/eJyrVspLzE1VslIKLi1ILUrLKc1MUfDJLARSmSWVSjpKOYmVqUVA6eoYpYoYJStLcwOdGKVKIMvIEsQqSa0oAXJilBTQwKMpe0hCMTF5WIwISUxPT00JqSxITUzKScWpDDuatotYm5CMwpQl1SdwhMsWqoUSmm9BwZQCjztg0GENAeJdgm7io%2BkthJxDcXDhdSBZQYY1XvHHDmFH4JADWkK29zHCgUomURYjlFgPCY3QvJLEAvSEifClhrNmCFZ5W1tbOzsFbAbAXYapl%2B5x4DDAMYDPfjwFHQnJGn9yx6%2BAShFBCxsIut0xrxJ7wiTgIkx9Qz2klGqVagHvSCn9)
--
-- == Known Sub-systems
--
-- * Agreements (ITA, CFA, IDA, etc.)
-- * Buffer Based Solvency (BBS)
-- * Composition Credit System
--
module Money.Systems.Superfluid.Concepts.TypedValue
    -- Untyped Value
    ( Value
    -- Typed Value
    , TypedValueTag (..)
    , TypedValue (..)
    -- Untapped Value
    , UntappedValueTag
    , untappedValueTag
    , UntappedValue (..)
    -- Tapped Value
    , TappedValueTag
    , TappedValue (..)
    -- Any tapped Value
    , AnyTappedValue (..)
    , mkAnyTappedValue
    , untypeAnyTappedValue
    ) where

import           Data.Coerce               (Coercible, coerce)
import           Data.Default              (Default (..))
import           Data.Typeable             (Proxy (..), Typeable)

import           Money.Theory.Distribution (Value)

-- | Typed value is an otherwise unaccounted value ~v~ with an associated ~vtag~.
--
-- Notional conventions:
--  * Type name: tv
class (TypedValueTag vtag, Value v) => TypedValue tv vtag v | tv -> v, tv -> vtag where
    untypeValue :: tv -> v
    default untypeValue :: Coercible tv v => tv -> v
    untypeValue = coerce

-- | Tag for typed value type class using ~Typeable~ runtime information.
--
-- Notional conventions:
--  * Type name: vtag
class Typeable vtag => TypedValueTag vtag where tappedValueTag :: Proxy vtag -> String

-- | Untapped value tag.
data UntappedValueTag

instance TypedValueTag UntappedValueTag where tappedValueTag _ = "_"

-- | Create untapped value tag.
untappedValueTag :: Proxy UntappedValueTag
untappedValueTag = Proxy @UntappedValueTag

-- | Untapped value type. It does not have a designated sub system, and can be freely accessed by any sub systems.
--
-- Notional conventions:
--  * Term name: uval
newtype UntappedValue v = UntappedValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, Value)

instance Value v => TypedValue (UntappedValue v) UntappedValueTag v

-- | Tapped value tag. It must only be accessed by a designated sub system.
class TypedValueTag tvtag => TappedValueTag tvtag

-- | Tapped value type.
--
-- Notional conventions:
--  * Term name: tval
newtype TappedValue vtag v = TappedValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, Value)

instance (Value v, TypedValueTag vtag) => TypedValue (TappedValue vtag v) vtag v

-- | Any tapped value Type
--
-- Notional conventions:
--  * Term name: aval
data AnyTappedValue v = forall vtag. TappedValueTag vtag => AnyTappedValue (Proxy vtag, v)

-- | Create any tapped value
mkAnyTappedValue
    :: forall vtag v. (Value v, TappedValueTag vtag)
    => TappedValue vtag v -> AnyTappedValue v
mkAnyTappedValue (TappedValue uval) = AnyTappedValue (Proxy @vtag, uval)

-- | Untype any tapped value
untypeAnyTappedValue :: AnyTappedValue v -> v
untypeAnyTappedValue (AnyTappedValue (_, uval)) = uval
