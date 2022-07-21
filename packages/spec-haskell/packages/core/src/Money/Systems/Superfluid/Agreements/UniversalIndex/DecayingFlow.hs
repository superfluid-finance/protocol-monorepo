{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.UniversalIndex.DecayingFlow where

import           Data.Default
import           Data.Proxy
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow as DFMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency           as BBS

--
import           Money.Systems.Superfluid.Agreements.UniversalIndex.Data

-- * Monetary data lenses
--
instance SuperfluidTypes sft => DFMUD.MonetaryUnitLenses (UniversalIndex sft) sft where
    decayingFactor = readOnlyLens (\_ -> dfa_default_lambda (Proxy @sft))
    settledAt      = $(field 'dfa_settledAt)
    αVal           = $(field 'dfa_αVal)
    εVal           = $(field 'dfa_εVal)
    settledBuffer  = $(field 'dfa_settledBuffer)
type DFAMonetaryUnitData sft = DFMUD.MonetaryUnitData (UniversalIndex sft) sft

-- * Contract
--

data DFAContractData sft = DFAContractData
    { dfa_flow_last_updated_at :: SFT_TS sft
    , dfa_distribution_limit   :: SFT_MVAL sft
    , dfa_flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DFAContractData sft)
instance SuperfluidTypes sft => AgreementContractData (DFAContractData sft) sft

-- * Operation

type DistributionLimit sft = SFT_MVAL sft

data DFAOperation sft =
    --                 θ/distributionLimit     newFlowBuffer
    UpdateDecayingFlow (DistributionLimit sft) (BBS.BufferValue (SFT_MVAL sft))

instance SuperfluidTypes sft => AgreementOperation (DFAOperation sft)
    (DFAContractData sft) (DFAMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (DFAOperation sft) elem = DFAContractPartiesF
        { decayingFlowSender   :: elem
        , decayingFlowReceiver :: elem
        } deriving stock (Functor, Foldable, Traversable)

    -- | Create data of agreement parties from the changes of the contract.
    --
    -- Formula:
    --   aad_mempty_update_with_acd(aad, θ_Δ, t_u) = DFAAAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation (UpdateDecayingFlow θ newFlowBuffer) acd t' = let
        acd'  = DFAContractData { dfa_distribution_limit   = θ
                                , dfa_flow_buffer          = newFlowBuffer
                                , dfa_flow_last_updated_at = t'
                                }
        aopssΔ = fmap DFMUD.MkMonetaryUnitData (DFAContractPartiesF
                    (def & set DFMUD.settledAt     t'
                         & set DFMUD.αVal          θ_Δ
                         & set DFMUD.εVal          (-θ_Δ)
                         & set DFMUD.settledBuffer flowBufferDelta)
                    (def & set DFMUD.settledAt     t'
                         & set DFMUD.αVal          (-θ_Δ)
                         & set DFMUD.εVal          θ_Δ))
        in (acd', aopssΔ)
        where
            θ_Δ             = fromIntegral (θ - (dfa_distribution_limit acd))
            flowBufferDelta = newFlowBuffer - (dfa_flow_buffer acd)
