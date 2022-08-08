{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant transferring agreement.
--
-- This module is typically imported using qualified name ITA.
module Money.Systems.Superfluid.Agreements.InstantTransferAgreement where

import           Data.Coerce
import           Data.Default
import           Data.Kind
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue as IVMUD

-- * Monetary data lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { untapped_value         :: UntappedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MonetaryUnitLenses sft)

instance SuperfluidTypes sft => IVMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    untappedValue = $(field 'untapped_value)
type MonetaryUnitData sft = IVMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft

-- * Contract
--

-- * Operation
--

newtype Operation sft = Transfer (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (Operation sft) sft where
    data AgreementOperationData (Operation sft) = ContractData

    data AgreementOperationResultF (Operation sft) elem = OperationPartiesF
        { transferFrom :: elem
        , transferTo   :: elem
        } deriving stock (Functor, Foldable, Traversable)

    type AgreementMonetaryUnitDataInOperation (Operation sft) = MonetaryUnitData sft

    applyAgreementOperation (Transfer amount) acd _ = let
        acd'  = acd
        aorΔ = fmap IVMUD.MkMonetaryUnitData (OperationPartiesF
                    (def & set IVMUD.untappedValue (coerce (- amount)))
                    (def & set IVMUD.untappedValue (coerce    amount)))
        in (acd', aorΔ)

type ContractData :: Type -> Type
type ContractData sft = AgreementOperationData (Operation sft)

instance SuperfluidTypes sft => Default (ContractData sft) where def = ContractData
