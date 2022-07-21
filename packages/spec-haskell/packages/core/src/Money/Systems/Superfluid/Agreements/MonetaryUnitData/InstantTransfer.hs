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

class (Default amudL, SuperfluidTypes sft) => MonetaryUnitLenses amudL sft | amudL -> sft where
    untappedValue :: Lens' amudL (UntappedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amudL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amudL } deriving (Default)

instance MonetaryUnitLenses amudL sft => Semigroup (MonetaryUnitData amudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amudL sft => Monoid (MonetaryUnitData amudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amudL sft => AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB (a^.untappedValue) []
