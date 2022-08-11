{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Constant flow agreement.
--
-- This module is typically imported using qualified name CFA.
module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement where

import           Data.Default
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow as CFMUD

-- * Monetary unit lenses.
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { settled_at    :: SFT_TS sft
    , settled_value :: UntappedValue (SFT_MVAL sft)
    , net_flow_rate :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MonetaryUnitLenses sft)

-- | Monetary unit lenses for the universal index.
instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    settledAt          = $(field 'settled_at)
    settledValue       = $(field 'settled_value)
    netFlowRate        = $(field 'net_flow_rate)

-- | Type alias for the constant flow monetary unit data.
type MonetaryUnitData sft = CFMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft

-- * Contract
--

-- * Operation
--

data ContractData sft = ContractData
    { flow_updated_at :: SFT_TS sft -- TODO, useless field, move to effect stage
    , flow_rate       :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ContractData sft)

type FlowRate sft = SFT_MVAL sft

instance SuperfluidTypes sft => AgreementContract (ContractData sft) sft where
    applyAgreementOperation ac (UpdateFlow newFlowRate) t' = let
        fr        = flow_rate ac
        flowRateΔ = newFlowRate - fr

        ac' = ContractData { flow_updated_at = t'
                            , flow_rate       = newFlowRate
                            }

        muds = OperationOutputF
                   (def & set CFMUD.settledAt t'
                        & set CFMUD.netFlowRate  (-flowRateΔ)
                   )
                   (def & set CFMUD.settledAt t'
                        & set CFMUD.netFlowRate  flowRateΔ
                   )
        in (ac', fmap CFMUD.MkMonetaryUnitData muds)

    functorizeAgreementOperationOutput muds = fmap MkMonetaryUnitDataClass muds

    data AgreementOperation (ContractData sft) = UpdateFlow (FlowRate sft)

    type AgreementOperationOutput (ContractData sft) =
        AgreementOperationOutputF (ContractData sft)
        (MonetaryUnitData sft)

    data AgreementOperationOutputF (ContractData sft) a = OperationOutputF
        { flow_sender   :: a
        , flow_receiver :: a
        } deriving stock (Functor, Foldable, Traversable)
