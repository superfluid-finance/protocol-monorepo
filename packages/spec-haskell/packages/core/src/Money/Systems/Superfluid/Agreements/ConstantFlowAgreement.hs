{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Constant flow agreement.
--
-- This module is typically imported using qualified name CFA.
module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement where

import           Data.Default
import           Data.Kind                                                         (Type)
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow as CFMUD

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

type FlowRate sft = SFT_MVAL sft

data Operation sft = UpdateFlow (FlowRate sft)

instance SuperfluidTypes sft => AgreementOperation (Operation sft) sft where
    data AgreementOperationData (Operation sft) = ContractData
        { flow_updated_at :: SFT_TS sft -- TODO, useless field, move to effect stage
        , flow_rate       :: SFT_MVAL sft
        }

    data AgreementOperationResultF (Operation sft) a = OperationPartiesF
        { flowSender   :: a
        , flowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    type AgreementMonetaryUnitDataInOperation (Operation sft) = MonetaryUnitData sft

    applyAgreementOperation (UpdateFlow newFlowRate) acd t' = let
        acd' = ContractData { flow_updated_at = t'
                            , flow_rate       = newFlowRate
                            }
        aorΔ = OperationPartiesF
                   (def & set CFMUD.settledAt t'
                        & set CFMUD.netFlowRate        (-flowRateΔ)
                   )
                   (def & set CFMUD.settledAt t'
                        & set CFMUD.netFlowRate        flowRateΔ
                   )
        in (acd', fmap CFMUD.MkMonetaryUnitData aorΔ)
        where fr          = flow_rate acd
              flowRateΔ   = newFlowRate - fr

type ContractData :: Type -> Type
type ContractData sft = AgreementOperationData (Operation sft)

-- NOTE: Unavoidable boilerplate due to the mysterious "No family instance for"
instance SuperfluidTypes sft => Default (ContractData sft) where
    def = ContractData { flow_updated_at = def, flow_rate = def }
