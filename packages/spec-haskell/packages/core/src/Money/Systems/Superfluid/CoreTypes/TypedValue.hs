{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | = Value Concept Refined for Superfluid Money
--
-- == Terminologies
--
-- - In /Superfluid Money Concepts/, plain 'Value' is also called "untyped" value, while 'TypedValue' is 'Value' that is
--   further typed denoting its purpose.
--
-- - Sub systems split "untyped" values into different types of typed value for the purposes of sub systems.
--
-- - UntappedValue is a special type of 'TypedValue' that its purpose is to signal that the value is "untapped", hence
--   readily to be used by any 'sub-system'.
--
module Money.Systems.Superfluid.CoreTypes.TypedValue
    ( Timestamp
    , MonetaryValue
    , TypedValue (..)
    , UntappedValue (..)
    , AnyTypedValue (..)
    , mkAnyTypedValue
    , exAnyTypedValue
    ) where

import           Data.Coerce               (Coercible, coerce)
import           Data.Default              (Default (..))
import           Data.Typeable             (Proxy (..), Typeable)

import           Money.Theory.MoneyDistribution (Timestamp, MonetaryValue)


-- | Typed value is an otherwise unaccounted value ~v~ with a type.
--
-- Notional conventions:
--  * Type name: tv
class (Typeable tv, MonetaryValue mv, Coercible tv mv) => TypedValue tv mv | tv -> mv where
    -- | Get string representation of typed value tag.
    typedValueTag :: Proxy tv -> String

-- | Untapped value type. It does not have a designated purpose controlled a specific sub system.
--   Hence any sub system may freely tap into it.
--
-- Notional conventions:
--  * Term name: uval
newtype UntappedValue v = MkUntappedValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, MonetaryValue)
instance (Typeable v, MonetaryValue v) => TypedValue (UntappedValue v) v where typedValueTag _ = "_"

-- | Any typed value.
--
-- Notional conventions:
--  * Term name: anyv
data AnyTypedValue v = forall tv. TypedValue tv v => AnyTypedValue (Proxy tv, tv)

-- | Create an existential typed value with proxy attached.
mkAnyTypedValue :: forall tv v. TypedValue tv v => tv -> AnyTypedValue v
mkAnyTypedValue tval = AnyTypedValue (Proxy @tv, tval)

-- | Extract raw monetary value from any typed value.
exAnyTypedValue :: AnyTypedValue v -> v
exAnyTypedValue (AnyTypedValue (_, tval)) = coerce tval
