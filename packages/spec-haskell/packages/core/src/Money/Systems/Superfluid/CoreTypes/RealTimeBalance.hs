{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.CoreTypes.RealTimeBalance where

import           Data.Default
import           Data.Typeable

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
class ( MonetaryValue v
      , Foldable rtbF
      , Monoid (rtbF v)
      , Eq (rtbF v)
      ) => RealTimeBalance rtbF v | rtbF -> v where
    -- | Convert a single monetary value to a RTB value.
    valueToRTB :: Proxy rtbF -> v -> rtbF v

    -- | Net monetary value of a RTB value.
    netValueOfRTB :: rtbF v -> v
    netValueOfRTB = foldr (+) def

    -- | Convert typed values to a RTB value.
    typedValuesToRTB :: [AnyTypedValue v] -> rtbF v

    -- | Get typed values from a RTB value.
    typedValuesFromRTB :: rtbF v -> [AnyTypedValue v]

-- =====================================================================================================================
-- * RealTimeBalance Laws

rtb_prop_mappend_commutativity :: forall rtbF v. RealTimeBalance rtbF v => rtbF v -> rtbF v -> Bool
rtb_prop_mappend_commutativity a b = (a <> b) == (b <> a)

rtb_prop_identity_from_and_to_typed_values :: forall rtbF v. RealTimeBalance rtbF v => rtbF v -> Bool
rtb_prop_identity_from_and_to_typed_values x = (typedValuesToRTB . typedValuesFromRTB) x == x

rtb_prop_conservation_of_net_value :: forall rtbF v. RealTimeBalance rtbF v => Proxy rtbF -> v -> Bool
rtb_prop_conservation_of_net_value p nv = (netValueOfRTB . valueToRTB p) nv == nv

rtb_prop_untapped_value_to_rtb :: forall rtbF v. (Typeable v, RealTimeBalance rtbF v) => Proxy rtbF -> v -> Bool
rtb_prop_untapped_value_to_rtb p nv = valueToRTB p nv == typedValuesToRTB [(mkAnyTypedValue . MkUntappedValue) nv]
