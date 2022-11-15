{-# LANGUAGE DerivingVia #-}

module Money.Systems.Superfluid.SubSystems.BufferBasedSolvency
    ( BufferValue (..)
    ) where

import           Data.Default
import           Data.Typeable

import           Money.Systems.Superfluid.CoreTypes


newtype BufferValue v = MkBufferValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, MonetaryValue)
instance (Typeable v, MonetaryValue v) => TypedValue (BufferValue v) v where typedValueTag _ = "b"
