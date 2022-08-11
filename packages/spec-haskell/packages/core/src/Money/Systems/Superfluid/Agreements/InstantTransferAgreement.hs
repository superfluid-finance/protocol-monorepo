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
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.MonetaryUnitData.InstantValue as IVMUD

-- * Monetary data lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { untapped_value         :: UntappedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MonetaryUnitLenses sft)

instance SuperfluidTypes sft => IVMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    untappedValue = $(field 'untapped_value)
type MonetaryUnitData sft = IVMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft

-- * Contract & Operation
--

-- No ongoing relationships between parties
data ContractData sft = ContractData

instance SuperfluidTypes sft => Default (ContractData sft) where def = ContractData
instance SuperfluidTypes sft => MonetaryUnitDataClass (ContractData sft) sft where

instance SuperfluidTypes sft => AgreementContract (ContractData sft) sft where
    applyAgreementOperation ac (Transfer amount) _ = let
        ac' = ac
        mudsΔ = fmap IVMUD.MkMonetaryUnitData (OperationOutputF
                    (def & set IVMUD.untappedValue (coerce (- amount)))
                    (def & set IVMUD.untappedValue (coerce    amount)))
        in (ac', mudsΔ)

    functorizeAgreementOperationOutput muds = fmap MkMonetaryUnitDataClass muds

    data AgreementOperation (ContractData sft) = Transfer (SFT_MVAL sft)

    type AgreementOperationOutput (ContractData sft) =
        AgreementOperationOutputF (ContractData sft) (MonetaryUnitData sft)

    data AgreementOperationOutputF (ContractData sft) elem = OperationOutputF
        { transfer_from :: elem
        , transfer_to   :: elem
        } deriving stock (Functor, Foldable, Traversable)
