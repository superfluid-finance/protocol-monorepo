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
import qualified Money.Systems.Superfluid.MonetaryUnitData.DecayingFlow as DFMUD

-- * Monetary data lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { settled_at :: SFT_TS sft
    , α_val      :: SFT_FLOAT sft
    , ε_val      :: SFT_FLOAT sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MonetaryUnitLenses sft)

instance SuperfluidTypes sft => DFMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    decayingFactor = readOnlyLens (\_ -> dfa_default_lambda (Proxy @sft))
    settledAt      = $(field 'settled_at)
    αVal           = $(field 'α_val)
    εVal           = $(field 'ε_val)
type MonetaryUnitData sft = DFMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft

-- * Operation

type DistributionLimit sft = SFT_MVAL sft

data ContractData sft = ContractData
    { flow_last_updated_at :: SFT_TS sft
    , distribution_limit   :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ContractData sft)

instance SuperfluidTypes sft => MonetaryUnitDataClass (ContractData sft) sft where

instance SuperfluidTypes sft => AgreementContract (ContractData sft) sft where
    -- | Create data of agreement parties from the changes of the contract.
    --
    -- Formula:
    --   aad_mempty_update_with_ac(aad, θ_Δ, t_u) = AAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation ac (UpdateDecayingFlow θ) t' = let
        θ_Δ = fromIntegral (θ - distribution_limit ac)

        ac' = ContractData { distribution_limit   = θ
                            , flow_last_updated_at = t'
                            }
        mudsΔ = OperationOutputF
                (def & set DFMUD.settledAt    t'
                     & set DFMUD.αVal        θ_Δ
                     & set DFMUD.εVal      (-θ_Δ))
                (def & set DFMUD.settledAt    t'
                     & set DFMUD.αVal      (-θ_Δ)
                     & set DFMUD.εVal        θ_Δ)

        in (ac', fmap DFMUD.MkMonetaryUnitData mudsΔ)

    functorizeAgreementOperationOutput muds = fmap MkMonetaryUnitDataClass muds

    data AgreementOperation (ContractData sft) =
        UpdateDecayingFlow (DistributionLimit sft)

    type AgreementOperationOutput (ContractData sft) =
        AgreementOperationOutputF (ContractData sft)
        (MonetaryUnitData sft)

    data AgreementOperationOutputF (ContractData sft) elem = OperationOutputF
        { flow_sender   :: elem
        , flow_receiver :: elem
        } deriving stock (Functor, Foldable, Traversable)
