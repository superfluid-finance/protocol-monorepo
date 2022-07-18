{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.RealTimeBalance where

import           Data.Default
import           Data.Proxy
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts.TypedValue

-- | RealTimeBalance Type Class
--
-- Notional conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: bal, balance, ...
class ( Value v
      , Applicative rtbF, Traversable rtbF
      , Num (rtbF v)
      , Default (rtbF v)
      ) => RealTimeBalance rtbF v | v -> rtbF where

    -- * RTB Constructors
    --

    -- | Convert a single monetary value to a RTB
    --
    -- *Law*
    -- 1. ??
    valueToRTB :: v -> rtbF v

    -- | Convert typed values to a RTB
    typedValuesToRTB :: UntappedValue v -> [AnyTappedValue v] -> rtbF v

    -- * RTB Utils
    --

    -- | Get typed values from a RTB
    typedValuesFromRTB :: rtbF v -> (UntappedValue v, [AnyTappedValue v])

    -- | Get the lens of a typed value of RTB.
    lensOfRTB :: TypedValueTag vtag => Proxy vtag -> Lens' (rtbF v) v

    -- | Net monetary value of the RTB
    netValueOfRTB :: (Value v, RealTimeBalance rtb v) => rtbF v -> v
    netValueOfRTB = foldr (+) def

    -- | Apply a binary function onto a normalized RTB
    normalizeRTBWith :: (v -> v) -> rtbF v -> rtbF v
    normalizeRTBWith f = valueToRTB . f . netValueOfRTB
