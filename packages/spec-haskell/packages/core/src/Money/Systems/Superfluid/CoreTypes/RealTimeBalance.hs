{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.CoreTypes.RealTimeBalance where

import           Data.Default

import           Money.Systems.Superfluid.CoreTypes.TypedValue


-- | RealTimeBalance type Class requires two requires two type parameters:
--
--   - @rtbF@ - a foldable functor.
--
--   - @v@ - the monetary value type.
--
-- Functions should operate on the @rtb@ type, which is an alias to @rtbF v@.
--
-- Notional conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: bal, balance, ...
--
-- Instances and their @rtb@ values should satisfy the following:
--
-- [Monoid laws] Right identity, left identity, associativity.
-- [RTB's mappend commutativity] @x <> y@ = @y <> x@
-- [RTB's identity to and from typed values] @(typedValuesToRTB . typedValuesFromRTB) x@ = @x@
-- [RTB's conservation of net value] @(netValueOfRTB . valueToRTB . netValueOfRTB) v@ = @netValueOfRTB v@
class ( Value v
      , Foldable rtbF
      , Monoid (rtbF v)
      , Eq (rtbF v)
      ) => RealTimeBalance rtbF v | rtbF -> v, v -> rtbF where

    -- | Convert a single monetary value to a RTB value.
    valueToRTB :: v -> rtbF v

    -- | Net monetary value of a RTB value.
    netValueOfRTB :: rtbF v -> v
    netValueOfRTB = foldr (+) def

    -- | Convert typed values to a RTB value.
    typedValuesToRTB :: UntappedValue v -> [AnyTappedValue v] -> rtbF v

    -- | Get typed values from a RTB value.
    typedValuesFromRTB :: rtbF v -> (UntappedValue v, [AnyTappedValue v])
