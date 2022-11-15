{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.MonetaryUnitData.InstantValue
    ( MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                         (Default (..))
import           Data.Kind                            (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes

-- * Monetary unit data
--

class ( Default amuLs
      , SuperfluidSystemTypes sft
      ) => MonetaryUnitLenses amuLs sft | amuLs -> sft where
    untappedValue :: Lens' amuLs (UntappedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amuLs sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuLs } deriving (Default)

instance MonetaryUnitLenses amuLs sft => Semigroup (MonetaryUnitData amuLs sft) where
    MkMonetaryUnitData a <> MkMonetaryUnitData b =
        let c = a & over untappedValue (+ b^.untappedValue)
        in MkMonetaryUnitData c

instance MonetaryUnitLenses amuLs sft => MonetaryUnitDataClass (MonetaryUnitData amuLs sft) sft where
    balanceProvided (MkMonetaryUnitData a) _ = typedValuesToRTB [mkAnyTypedValue $ a^.untappedValue]
