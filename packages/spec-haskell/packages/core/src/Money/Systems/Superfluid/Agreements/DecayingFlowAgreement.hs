{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

-- | Decaying Flow Agreement (DFA)
--
-- NOTE: The formulas can be obtained from the output of maths/DFA.py
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement
    ( AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractData
    , AccountData
    , ContractPartiesF
    , ContractParties
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Data.Type.TaggedTypeable

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


default_lambda :: RealFloat b => b
default_lambda = log 2 / (3600 * 24 * 7)

-- Agreement Definition
--
type DFA :: Type -> Type -- kind signature is required to make GHC happy
data DFA sft

instance SuperfluidTypes sft => Agreement (DFA sft) sft where
    data AgreementContractData (DFA sft) = ContractData
        { flowLastUpdatedAt :: SFT_TS sft
        , decayingFactor    :: SFT_FLOAT sft
        , distributionLimit :: SFT_LQ sft
        , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
        }

    data AgreementAccountData (DFA sft) = AccountData
        { settledAt     :: SFT_TS sft
        , αVal          :: SFT_FLOAT sft
        , εVal          :: SFT_FLOAT sft
        , settledBuffer :: BBS.BufferLiquidity (SFT_LQ sft)
        }

    data AgreementContractPartiesF (DFA sft) a = ContractPartiesF
        { decayingFlowSender   :: a
        , decayingFlowReceiver :: a
        } deriving stock (Functor, Foldable)

    data AgreementOperation (DFA sft) =
        -- θ (Distribution Limit), newFlowBuffer, t'
        UpdateDecayingFlow (SFT_LQ sft) (BBS.BufferLiquidity (SFT_LQ sft)) (SFT_TS sft)

    -- | Provided balance by DFA
    --
    -- Formula:
    --   rtb(aad, t) = α * e ^ (-λ * (t - t_s)) + ε
    --       where { t_s = t_s, αVal = α, εVal = ε } = aad
    balanceProvidedByAgreement AccountData
        { settledAt = t_s, αVal = α, εVal = ε, settledBuffer = buf_s } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ ceiling $ α * exp (-λ * t_Δ) + ε )
            [ mkAnyTappedLiquidity buf_s ]
        where λ = default_lambda
              t_Δ = fromIntegral (t - t_s)

    -- | Create data of agreement parties from the changes of the DFA contract
    --
    -- Formula:
    --   aad_mempty_update_with_acd(aad, θ_Δ, t_u) = DFA_AAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation acd (UpdateDecayingFlow θ newFlowBuffer t') = let
        acd' = acd { distributionLimit = θ, flowBuffer = newFlowBuffer, flowLastUpdatedAt = t' }
        acps' = ContractPartiesF AccountData { settledAt = t', αVal = θ_Δ, εVal = -θ_Δ, settledBuffer = flowBufferDelta }
                           AccountData { settledAt = t', αVal = -θ_Δ, εVal = θ_Δ, settledBuffer = def }
        in (acd', acps')
        where
            θ_Δ = fromIntegral (θ - distributionLimit acd)
            flowBufferDelta = newFlowBuffer - flowBuffer acd

type ContractData sft = AgreementContractData (DFA sft)
type AccountData sft = AgreementAccountData (DFA sft)
type ContractPartiesF sft = AgreementContractPartiesF (DFA sft)
type ContractParties sft = (ContractPartiesF sft) (AccountData sft)

instance SuperfluidTypes sft => Applicative (ContractPartiesF sft) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')

instance SuperfluidTypes sft => TaggedTypeable (ContractData sft) where tagFromProxy _ = "DFA#"

instance SuperfluidTypes sft => Default (ContractData sft) where
    def = ContractData
        { flowLastUpdatedAt = def
        , decayingFactor = default_lambda
        , distributionLimit = def
        , flowBuffer = def
        }

instance SuperfluidTypes sft => TaggedTypeable (AccountData sft) where tagFromProxy _ = "DFA"
instance SuperfluidTypes sft => Default (AccountData sft) where
    def = AccountData
        { settledAt = def
        , αVal = def
        , εVal = def
        , settledBuffer = def
        }
instance SuperfluidTypes sft => Semigroup (AccountData sft) where
    -- Formula:
    --   aad_mappend(a, b) = DFA_AAD
    --     { t_s = t_s'
    --     , α = α * e ^ (λ * (t_s - t_s')) - ε'
    --     , ε = ε + ε'
    --     }
    --     where { t_s = t_s, αVal = α, εVal = ε } = a
    --           { t_s = t_s', αVal = α', εVal = ε' } = b
    (<>) a b = AccountData
        { settledAt = settledAt b
        , αVal = (αVal a) * exp (-λ * t_Δ) - εVal b
        , εVal = εVal a + εVal b
        , settledBuffer = settledBuffer a + settledBuffer b
        }
        where λ = default_lambda
              t_Δ = fromIntegral (settledAt b - settledAt a)
