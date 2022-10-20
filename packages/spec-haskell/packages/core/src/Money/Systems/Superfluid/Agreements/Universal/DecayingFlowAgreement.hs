{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Decaying flow agreement.
--
-- This module is typically imported using qualified name .
module Money.Systems.Superfluid.Agreements.Universal.DecayingFlowAgreement where

import           Data.Default
import           Data.Proxy
import           Data.Type.Any
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.MonetaryUnitData.DecayingFlow as DFMUD


-- * Monetary data lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { settled_at :: SFT_TS sft
    , α_val      :: SFT_FLOAT sft
    , ε_val      :: SFT_FLOAT sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (MonetaryUnitLenses sft)

type MonetaryUnitData sft = DFMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft
instance SuperfluidSystemTypes sft => SemigroupMonetaryUnitData (MonetaryUnitData sft) sft

instance SuperfluidSystemTypes sft => DFMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    decayingFactor = readOnlyLens (\_ -> dfa_default_lambda (Proxy @sft))
    settledAt      = $(field 'settled_at)
    αVal           = $(field 'α_val)
    εVal           = $(field 'ε_val)

-- * Contract

type DistributionLimit sft = SFT_MVAL sft

data ContractData sft = ContractData
    { flow_last_updated_at :: SFT_TS sft
    , distribution_limit   :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (ContractData sft)

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (ContractData sft) sft

-- * Operation

instance SuperfluidSystemTypes sft => AgreementContract (ContractData sft) sft where
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

    functorizeAgreementOperationOutput p = fmap (mkAny p)

    data AgreementOperation (ContractData sft) =
        UpdateDecayingFlow (DistributionLimit sft)

    type AgreementOperationOutput (ContractData sft) = OperationOutput sft

    data AgreementOperationOutputF (ContractData sft) elem = OperationOutputF
        { flow_sender   :: elem
        , flow_receiver :: elem
        } deriving stock (Functor, Foldable, Traversable, Generic)

type OperationOutput sft = AgreementOperationOutputF (ContractData sft) (MonetaryUnitData sft)

instance SuperfluidSystemTypes sft => Default (OperationOutput sft)
instance SuperfluidSystemTypes sft => Monoid (OperationOutput sft) where mempty = def
instance SuperfluidSystemTypes sft => Semigroup (OperationOutput sft) where
    OperationOutputF a b <> OperationOutputF a' b' = OperationOutputF (a <> a') (b <> b')
