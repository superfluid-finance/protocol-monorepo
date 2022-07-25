{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue
    ( MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts

-- * Monetary unit data
--

class (Default amuLs, SuperfluidTypes sft) => MonetaryUnitLenses amuLs sft | amuLs -> sft where
    untappedValue :: Lens' amuLs (UntappedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amuLs sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuLs } deriving (Default)

instance MonetaryUnitLenses amuLs sft => Semigroup (MonetaryUnitData amuLs sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amuLs sft => Monoid (MonetaryUnitData amuLs sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amuLs sft => AgreementMonetaryUnitData (MonetaryUnitData amuLs sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB (a^.untappedValue) []
