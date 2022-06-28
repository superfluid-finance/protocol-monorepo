{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


-- | Decaying Flow Agreement (DFA)
--
-- The formulas are taken from output from maths/DFA.py
--
--   DFA_AAD(acc) = { α, t_s, ε }
--   DFA_RTB(aac, t) =  α * e ^ (-λ * (t - t_s)) + ε
--       where { α, t_s, ε } = DFA_AAD(acc)
--   DFA_UPDATE(aac, θ, t_u) =  modify
--       DFA_AAD { α' = α * e ^ (λ * (t_s - t_u)) + θ
--               , t_s' = t_u
--               , ε' = -θ + ε }
--
--   Half-life of distriibution :
--
--   t_h == log(2) / λ
--   λ = log(2) /  t_h
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement
    ( DFAContractData (..)
    , DFAAccountData (..)
    , DecayingFlow (..)
    , updateDecayingFlow
    ) where

import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Text.Printf                                             (printf)

import           Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementAccountData (..)
    , AgreementContractData
    )
import           Money.Systems.Superfluid.Concepts.Liquidity             (UntappedLiquidity (..), mkAnyTappedLiquidity)
import           Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( RealtimeBalance (..)
    , TypedLiquidityVector (..)
    )
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes
--
import           Data.Internal.TaggedTypeable
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS

default_lambda :: RealFloat b => b
default_lambda = log 2 / (3600 * 24 * 7)


-- | DFAAccountData Type (is AgreementAccountData)
--
type DFAAccountData :: Type -> Type
data DFAAccountData sft = DFAAccountData
    { settledAt     :: SFT_TS sft
    , αVal          :: SFT_FLOAT sft
    , εVal          :: SFT_FLOAT sft
    , settledBuffer :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => TaggedTypeable (DFAAccountData sft) where tagFromProxy _ = "DFA"

instance SuperfluidTypes sft => AgreementAccountData (DFAAccountData sft) sft where
    providedBalanceOfAgreement DFAAccountData
        { settledAt = t_s
        , αVal = α
        , εVal = ε
        , settledBuffer = buf_s
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ ceiling $ α * exp (-λ * t_Δ) + ε )
            [ mkAnyTappedLiquidity buf_s ]
        where λ = default_lambda
              t_Δ = fromIntegral (t - t_s)

instance SuperfluidTypes sft => Show (DFAAccountData sft) where
    show x = printf "{ t_s = %s, α = %s, ε = %s, buf = %s }"
        (show $ settledAt x)
        (show $ αVal x)
        (show $ εVal x)
        (show $ settledBuffer x)

instance SuperfluidTypes sft => Semigroup (DFAAccountData sft) where
    (<>) = undefined
instance SuperfluidTypes sft => Monoid (DFAAccountData sft) where
    mempty = DFAAccountData
        { settledAt = def
        , αVal = def
        , εVal = def
        , settledBuffer = def }

-- | DFAContractData Type
--
type DFAContractData :: Type -> Type
data DFAContractData sft = DFAContractData
    { flowLastUpdatedAt :: SFT_TS sft
    , decayingFactor    :: SFT_FLOAT sft
    , distributionLimit :: SFT_LQ sft
    , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => TaggedTypeable (DFAContractData sft) where tagFromProxy _ = "DFA#"
instance SuperfluidTypes sft => Default (DFAContractData sft) where
    def = DFAContractData
        { flowLastUpdatedAt = def
        , decayingFactor = def
        , distributionLimit = def
        , flowBuffer = def
        }

instance SuperfluidTypes sft => Show (DFAContractData sft) where
    show x = printf "{ t_u = %s, δ = %s, λ = %s }"
        (show $ flowLastUpdatedAt x) (show $ distributionLimit x) (show $ decayingFactor x)

instance SuperfluidTypes sft => AgreementContractData (DFAContractData sft) sft (DFAAccountData sft)

-- ============================================================================

-- ============================================================================
-- DFA Operations
--
data DecayingFlow sft = DecayingFlow
    { flowContract :: DFAContractData sft
    , flowSender   :: DFAAccountData sft
    , flowReceiver :: DFAAccountData sft
    }

updateDecayingFlow
    :: SuperfluidTypes sft
    => DecayingFlow sft -> SFT_LQ sft -> BBS.BufferLiquidity (SFT_LQ sft) -> SFT_TS sft -> DecayingFlow sft
updateDecayingFlow
    (DecayingFlow cfaACD senderAAD receiverAAD)
    newDistributionLimit
    newFlowBuffer t =
    DecayingFlow DFAContractData
            { flowLastUpdatedAt = t
            , distributionLimit = newDistributionLimit
            , decayingFactor = λ
            , flowBuffer = newFlowBuffer
            }
        ( update_aad senderAAD distributionLimitDelta )
        ( update_aad receiverAAD (negate distributionLimitDelta) )
    where
    λ = default_lambda
    distributionLimitDelta = newDistributionLimit - distributionLimit cfaACD
    flowBufferDelta = newFlowBuffer - flowBuffer cfaACD
    update_aad DFAAccountData
        { settledAt = t_s
        , αVal = α
        , εVal = ε
        , settledBuffer = buf_s
        } θ_Δ
        = DFAAccountData
        { settledAt = t
        , αVal = α * exp (-λ * t_Δ) + fromIntegral θ_Δ
        , εVal = ε - fromIntegral θ_Δ
        , settledBuffer = buf_s + flowBufferDelta
        }
        where
        t_Δ = fromIntegral (t - t_s)
