{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.RealTimeBalance where

import           Data.Default
import           Data.Proxy
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts.TypedValue

-- | RealTimeBalance type Class requires two requires two type parameters, @rtbF@ functor and monetary value type @v@.
--
-- Functions operate on the @rtb@ type, which is an alias to @rtbF v@.
--
-- Notional conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: bal, balance, ...
--
-- Instances and their @rtb@ values should satisfy the following:
--
-- [Commutativity] @x '<>' 'mempty' = x@
-- [RTB identity to and from typed values]@(typedValuesToRTB . typedValuesFromRTB) x = x@
-- [RTB conservation of net value] @(netValueOfRTB . valueToRTB . netValueOfRTB) v = netValueOfRTB v@
class ( Value v
      , Applicative rtbF, Traversable rtbF
      , Monoid (rtbF v)
      ) => RealTimeBalance rtbF v | v -> rtbF where

    -- | Convert a single monetary value to a RTB
    valueToRTB :: v -> rtbF v

    -- | Net monetary value of the RTB
    netValueOfRTB :: rtbF v -> v
    netValueOfRTB = foldr (+) def

    -- | Convert typed values to a RTB
    typedValuesToRTB :: UntappedValue v -> [AnyTappedValue v] -> rtbF v

    -- | Get typed values from a RTB
    typedValuesFromRTB :: rtbF v -> (UntappedValue v, [AnyTappedValue v])

    -- | Get the lens of a typed value of RTB.
    lensOfRTB :: TypedValueTag vtag => Proxy vtag -> Lens' (rtbF v) v
