{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Constant flow agreement.
--
-- This module is typically imported using qualified name CFA.
module Money.Systems.Superfluid.Agreements.Universal.ConstantFlowAgreement where

import           Data.Default
import           Data.Type.Any
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow as CFMUD


-- * Monetary unit lenses.
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { settled_at    :: SFT_TS sft
    , settled_value :: UntappedValue (SFT_MVAL sft)
    , net_flow_rate :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (MonetaryUnitLenses sft)

-- | Type alias for the constant flow monetary unit data.
type MonetaryUnitData sft = CFMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft
instance SuperfluidSystemTypes sft => SemigroupMonetaryUnitData (MonetaryUnitData sft) sft

-- | Monetary unit lenses for the universal index.
instance SuperfluidSystemTypes sft => CFMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    settledAt          = $(field 'settled_at)
    settledValue       = $(field 'settled_value)
    netFlowRate        = $(field 'net_flow_rate)

-- * Contract
--

type FlowRate sft = SFT_MVAL sft

data ContractData sft = ContractData
    { flow_updated_at :: SFT_TS sft -- TODO, useless field, move to effect stage
    , flow_rate       :: FlowRate sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (ContractData sft)

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (ContractData sft) sft

-- * Operation
--

instance SuperfluidSystemTypes sft => AgreementContract (ContractData sft) sft where
    applyAgreementOperation ac (UpdateFlow newFlowRate) t' = let
        fr        = flow_rate ac
        flowRateΔ = newFlowRate - fr

        ac' = ContractData { flow_updated_at = t'
                           , flow_rate       = newFlowRate
                           }

        mudsΔ = OperationOutputF
                (def & set CFMUD.settledAt t'
                     & set CFMUD.netFlowRate  (-flowRateΔ))
                (def & set CFMUD.settledAt t'
                     & set CFMUD.netFlowRate  flowRateΔ)
        in (ac', fmap CFMUD.MkMonetaryUnitData mudsΔ)

    functorizeAgreementOperationOutput p = fmap (mkAny p)

    data AgreementOperation (ContractData sft) = UpdateFlow (FlowRate sft)

    type AgreementOperationOutput (ContractData sft) = OperationOutput sft

    data AgreementOperationOutputF (ContractData sft) a = OperationOutputF
        { flow_sender   :: a
        , flow_receiver :: a
        } deriving stock (Functor, Foldable, Traversable, Generic)

type OperationOutput sft = AgreementOperationOutputF (ContractData sft) (MonetaryUnitData sft)

instance SuperfluidSystemTypes sft => Default (OperationOutput sft)
instance SuperfluidSystemTypes sft => Monoid (OperationOutput sft) where mempty = def
instance SuperfluidSystemTypes sft => Semigroup (OperationOutput sft) where
    OperationOutputF a b <> OperationOutputF a' b' = OperationOutputF (a <> a') (b <> b')
