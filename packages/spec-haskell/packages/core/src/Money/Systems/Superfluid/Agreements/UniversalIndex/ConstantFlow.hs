{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.UniversalIndex.ConstantFlow where

import           Data.Coerce
import           Data.Default
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow as CFMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency           as BBS
--
import           Money.Systems.Superfluid.Agreements.UniversalIndex.Data


-- * Monetary unit lenses.
--

-- | Monetary unit lenses for the universal index.
instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (UniversalData sft) sft where
    settledAt            = $(field 'cfa_settled_at)
    settledUntappedValue = $(field 'cfa_settled_untapped_value)
    settledBufferValue   = $(field 'cfa_settled_buffer_value)
    netFlowRate          = $(field 'cfa_net_flow_rate)

-- | Type alias for the constant flow monetary unit data.
type CFAMonetaryUnitData sft = CFMUD.MonetaryUnitData (UniversalData sft) sft

-- * Contract
--

-- | CFA contract data.
data CFAContractData sft = CFAContractData
    { cfa_flow_last_updated_at :: SFT_TS sft
    , cfa_flow_rate            :: SFT_MVAL sft
    , cfa_flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (CFAContractData sft)

-- * Operation
--

type FlowRate sft = SFT_MVAL sft

data CFAOperation sft =
    --         flowRate       newFlowBuffer
    UpdateFlow (FlowRate sft) (BBS.BufferValue (SFT_MVAL sft))

instance SuperfluidTypes sft => AgreementOperation (CFAOperation sft)
    (CFAContractData sft) (CFAMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (CFAOperation sft) a = CFAContractPartiesF
        { flowSender   :: a
        , flowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (UpdateFlow newFlowRate newFlowBuffer) acd t' = let
        acd'  = CFAContractData { cfa_flow_last_updated_at = t'
                                , cfa_flow_rate   = newFlowRate
                                , cfa_flow_buffer = newFlowBuffer
                                }
        aopssΔ = fmap CFMUD.MkMonetaryUnitData (CFAContractPartiesF
                    (def & set CFMUD.settledAt t'
                         & set CFMUD.netFlowRate (- flowRateDelta)
                         & set CFMUD.settledUntappedValue (UntappedValue $ (- flowPeriodDelta) - coerce flowBufferDelta)
                         & set CFMUD.settledBufferValue flowBufferDelta)
                    (def & set CFMUD.settledAt t'
                         & set CFMUD.netFlowRate flowRateDelta
                         & set CFMUD.settledUntappedValue (UntappedValue flowPeriodDelta)
                         & set CFMUD.settledBufferValue def))
        in (acd', aopssΔ)
        where t               = cfa_flow_last_updated_at acd
              fr              = cfa_flow_rate acd
              flowPeriodDelta = fr * fromIntegral (t' - t)
              flowRateDelta   = newFlowRate - fr
              flowBufferDelta = newFlowBuffer - cfa_flow_buffer acd
