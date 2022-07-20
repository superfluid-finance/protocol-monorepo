{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.InstantTransferAgreement
    ( MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    , ContractLens
    , ContractData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractPartiesF
    , ContractPartiesMUD
    ) where

import           Control.Applicative               (Applicative (..))
import           Data.Coerce                       (coerce)
import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts

-- * ITA.MonetaryUnitData
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

-- * ITA.ContractData
--

class (Default cdL, SuperfluidTypes sft) => ContractLens cdL sft | cdL -> sft

type ContractData :: Type -> Type -> Type -> Type
newtype ContractData cdL amudL sft = MkContractData { getContractLenses :: cdL } deriving (Default)

instance ( ContractLens cdL sft
         , MonetaryUnitLenses amudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft
         ) => AgreementContractData (ContractData cdL amudL sft) (MonetaryUnitData amudL sft) sft where

    data AgreementContractPartiesF (ContractData cdL amudL sft) a = ContractPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cdL amudL sft) =
        Transfer (SFT_MVAL sft)

    applyAgreementOperation acd (Transfer amount) _ = let
        acd'  = acd
        acpsΔ = fmap MkMonetaryUnitData (ContractPartiesF
                    (def & set untappedValue (coerce (- amount)))
                    (def & set untappedValue (coerce    amount)))
        in (acd', acpsΔ)

type ContractPartiesF   sft cdL amudL = AgreementContractPartiesF (ContractData cdL amudL sft)
type ContractPartiesMUD sft cdL amudL = ContractPartiesF sft cdL (MonetaryUnitData amudL sft)

instance Applicative (ContractPartiesF sft cdL amudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
