{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Constant flow agreement.
--
-- This module is typically imported using qualified name CFA.
module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement where

import           Data.Coerce
import           Data.Default
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow as CFMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency           as BBS
--
import           Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex

-- * Monetary unit lenses.
--

-- | Monetary unit lenses for the universal index.
instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (UniversalData sft) sft where
    settledAt            = $(field 'cfa_settled_at)
    settledUntappedValue = $(field 'cfa_settled_untapped_value)
    settledBufferValue   = $(field 'cfa_settled_buffer_value)
    netFlowRate          = $(field 'cfa_net_flow_rate)

-- | Type alias for the constant flow monetary unit data.
type MonetaryUnitData sft = CFMUD.MonetaryUnitData (UniversalData sft) sft

-- * Contract
--

-- |  contract data.
data ContractData sft = ContractData
    { flow_last_updated_at :: SFT_TS sft
    , flow_rate            :: SFT_MVAL sft
    , flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ContractData sft)

-- * Operation
--

type FlowRate sft = SFT_MVAL sft

data Operation sft =
    --         flowRate       newFlowBuffer
    UpdateFlow (FlowRate sft) (BBS.BufferValue (SFT_MVAL sft))

instance SuperfluidTypes sft => AgreementOperation (Operation sft)
    (ContractData sft) (MonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (Operation sft) a = OperationPartiesF
        { flowSender   :: a
        , flowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (UpdateFlow newFlowRate newFlowBuffer) acd t' = let
        acd'  = ContractData { flow_last_updated_at = t'
                             , flow_rate   = newFlowRate
                             , flow_buffer = newFlowBuffer
                             }
        aopsΔ = fmap CFMUD.MkMonetaryUnitData (OperationPartiesF
                    (def & set CFMUD.settledAt t'
                         & set CFMUD.netFlowRate (- flowRateDelta)
                         & set CFMUD.settledUntappedValue (UntappedValue $ (- flowPeriodDelta) - coerce flowBufferDelta)
                         & set CFMUD.settledBufferValue flowBufferDelta)
                    (def & set CFMUD.settledAt t'
                         & set CFMUD.netFlowRate flowRateDelta
                         & set CFMUD.settledUntappedValue (UntappedValue flowPeriodDelta)
                         & set CFMUD.settledBufferValue def))
        in (acd', aopsΔ)
        where t               = flow_last_updated_at acd
              fr              = flow_rate acd
              flowPeriodDelta = fr * fromIntegral (t' - t)
              flowRateDelta   = newFlowRate - fr
              flowBufferDelta = newFlowBuffer - flow_buffer acd
