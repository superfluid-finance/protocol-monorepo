{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer
    ( MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts

-- * Monetary unit data
--

class (Default amuL, SuperfluidTypes sft) => MonetaryUnitLenses amuL sft | amuL -> sft where
    untappedValue :: Lens' amuL (UntappedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amuL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuL } deriving (Default)

instance MonetaryUnitLenses amuL sft => Semigroup (MonetaryUnitData amuL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amuL sft => Monoid (MonetaryUnitData amuL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amuL sft => AgreementMonetaryUnitData (MonetaryUnitData amuL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB (a^.untappedValue) []
