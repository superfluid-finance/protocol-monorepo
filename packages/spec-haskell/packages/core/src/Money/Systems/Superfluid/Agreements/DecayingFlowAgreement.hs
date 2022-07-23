{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Decaying flow agreement.
--
-- This module is typically imported using qualified name .
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement where

import           Data.Default
import           Data.Proxy
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import           Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow as DFMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency           as BBS

-- * Monetary data lenses
--
instance SuperfluidTypes sft => DFMUD.MonetaryUnitLenses (UniversalData sft) sft where
    decayingFactor = readOnlyLens (\_ -> dfa_default_lambda (Proxy @sft))
    settledAt      = $(field 'dfa_settledAt)
    αVal           = $(field 'dfa_αVal)
    εVal           = $(field 'dfa_εVal)
    settledBuffer  = $(field 'dfa_settledBuffer)
type MonetaryUnitData sft = DFMUD.MonetaryUnitData (UniversalData sft) sft

-- * Contract
--

data ContractData sft = ContractData
    { flow_last_updated_at :: SFT_TS sft
    , distribution_limit   :: SFT_MVAL sft
    , flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ContractData sft)

-- * Operation

type DistributionLimit sft = SFT_MVAL sft

data Operation sft =
    --                 θ/distributionLimit     newFlowBuffer
    UpdateDecayingFlow (DistributionLimit sft) (BBS.BufferValue (SFT_MVAL sft))

instance SuperfluidTypes sft => AgreementOperation (Operation sft)
    (ContractData sft) (MonetaryUnitData sft) sft where
    data AgreementOperationResultF (Operation sft) elem = OperationPartiesF
        { decayingFlowSender   :: elem
        , decayingFlowReceiver :: elem
        } deriving stock (Functor, Foldable, Traversable)

    -- | Create data of agreement parties from the changes of the contract.
    --
    -- Formula:
    --   aad_mempty_update_with_acd(aad, θ_Δ, t_u) = AAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation (UpdateDecayingFlow θ newFlowBuffer) acd t' = let
        acd'  = ContractData { distribution_limit   = θ
                             , flow_buffer          = newFlowBuffer
                             , flow_last_updated_at = t'
                             }
        aorsΔ = fmap DFMUD.MkMonetaryUnitData (OperationPartiesF
                    (def & set DFMUD.settledAt     t'
                         & set DFMUD.αVal          θ_Δ
                         & set DFMUD.εVal          (-θ_Δ)
                         & set DFMUD.settledBuffer flowBufferDelta)
                    (def & set DFMUD.settledAt     t'
                         & set DFMUD.αVal          (-θ_Δ)
                         & set DFMUD.εVal          θ_Δ))
        in (acd', aorsΔ)
        where
            θ_Δ             = fromIntegral (θ - (distribution_limit acd))
            flowBufferDelta = newFlowBuffer - (flow_buffer acd)
