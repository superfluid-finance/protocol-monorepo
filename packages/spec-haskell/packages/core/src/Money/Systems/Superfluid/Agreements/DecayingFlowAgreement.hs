{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Decaying flow agreement.
--
-- This module is typically imported using qualified name .
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement where

import           Data.Default
import           Data.Kind                                                         (Type)
import           Data.Proxy
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow as DFMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency           as BBS

-- * Monetary data lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { settled_at     :: SFT_TS sft
    , α_val          :: SFT_FLOAT sft
    , ε_val          :: SFT_FLOAT sft
    , settled_buffer :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MonetaryUnitLenses sft)

instance SuperfluidTypes sft => DFMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    decayingFactor = readOnlyLens (\_ -> dfa_default_lambda (Proxy @sft))
    settledAt      = $(field 'settled_at)
    αVal           = $(field 'α_val)
    εVal           = $(field 'ε_val)
    settledBuffer  = $(field 'settled_buffer)
type MonetaryUnitData sft = DFMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft

-- * Operation

type DistributionLimit sft = SFT_MVAL sft

data Operation sft =
    --                 θ/distributionLimit     newFlowBuffer
    UpdateDecayingFlow (DistributionLimit sft) (BBS.BufferValue (SFT_MVAL sft))

instance SuperfluidTypes sft => AgreementOperation (Operation sft) sft where
    data AgreementContract (Operation sft) = ContractData
        { flow_last_updated_at :: SFT_TS sft
        , distribution_limit   :: SFT_MVAL sft
        , flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
        }
    data AgreementOperationResultF (Operation sft) elem = OperationResultF
        { decayingFlowSender   :: elem
        , decayingFlowReceiver :: elem
        } deriving stock (Functor, Foldable, Traversable)
    type MonetaryUnitDataInOperation (Operation sft) = MonetaryUnitData sft

    -- | Create data of agreement parties from the changes of the contract.
    --
    -- Formula:
    --   aad_mempty_update_with_acd(aad, θ_Δ, t_u) = AAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation (UpdateDecayingFlow θ newFlowBuffer) acd t' = let
        acd'  = ContractData { distribution_limit   = θ
                             , flow_buffer          = newFlowBuffer
                             , flow_last_updated_at = t'
                             }
        aorsΔ = OperationResultF
                    (def & set DFMUD.settledAt      t'
                         & set DFMUD.αVal           θ_Δ
                         & set DFMUD.εVal          (-θ_Δ)
                         & set DFMUD.settledBuffer flowBufferDelta
                    )
                    (def & set DFMUD.settledAt      t'
                         & set DFMUD.αVal          (-θ_Δ)
                         & set DFMUD.εVal           θ_Δ
                    )
        in (acd', fmap DFMUD.MkMonetaryUnitData aorsΔ)
        where
            θ_Δ             = fromIntegral (θ - distribution_limit acd)
            flowBufferDelta = newFlowBuffer - flow_buffer acd

type ContractData :: Type -> Type
type ContractData sft = AgreementContract (Operation sft)

-- NOTE: Unavoidable boilerplate due to the mysterious "No family instance for"
instance SuperfluidTypes sft => Default (ContractData sft) where
    def = ContractData { flow_last_updated_at = def, distribution_limit = def, flow_buffer = def }
