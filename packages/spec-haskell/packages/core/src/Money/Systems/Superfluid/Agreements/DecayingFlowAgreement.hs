{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

-- | Decaying Flow Agreement (DFA)
--
-- NOTE: The formulas can be obtained from the output of maths/DFA.py
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement
    ( AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementPartiesF (..)
    , AgreementOperation (..)
    , DFAContractData
    , DFAAccountData
    , DFAPartiesF
    , DFAParties
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

type DFA :: Type -> Type
data DFA sfd
type DFAContractData sfd = AgreementContractData (DFA sfd)
type DFAAccountData sfd = AgreementAccountData (DFA sfd)
type DFAPartiesF sfd = AgreementPartiesF (DFA sfd)
type DFAParties sfd = (DFAPartiesF sfd) (DFAAccountData sfd)
instance SuperfluidDistribution sfd => Agreement (DFA sfd) where
    type DistributionForAgreement (DFA sfd) = sfd

    data AgreementContractData (DFA sfd) = DFAContractData
        { flowLastUpdatedAt :: SFT_TS sfd
        , decayingFactor    :: SFT_FLOAT sfd
        , distributionLimit :: SFT_LQ sfd
        , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sfd)
        }

    data AgreementAccountData (DFA sfd) = DFAAccountData
        { settledAt     :: SFT_TS sfd
        , αVal          :: SFT_FLOAT sfd
        , εVal          :: SFT_FLOAT sfd
        , settledBuffer :: BBS.BufferLiquidity (SFT_LQ sfd)
        }

    data AgreementPartiesF (DFA sfd) a = DFAPartiesF a a deriving stock (Functor)

    data AgreementOperation (DFA sfd) =
        -- θ (Distribution Limit), newFlowBuffer, t'
        UpdateDecayingFlow (SFT_LQ sfd) (BBS.BufferLiquidity (SFT_LQ sfd)) (SFT_TS sfd)

    -- | Provided balance by DFA
    --
    -- Formula:
    --   rtb(aad, t) = α * e ^ (-λ * (t - t_s)) + ε
    --       where { t_s = t_s, αVal = α, εVal = ε } = aad
    providedBalanceByAgreement DFAAccountData
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
    createAgreementPartiesDelta acd (UpdateDecayingFlow θ newFlowBuffer t') = let
        acd' = acd { distributionLimit = θ, flowBuffer = newFlowBuffer, flowLastUpdatedAt = t' }
        aps' = DFAPartiesF DFAAccountData { settledAt = t', αVal = θ_Δ, εVal = -θ_Δ, settledBuffer = flowBufferDelta }
                           DFAAccountData { settledAt = t', αVal = -θ_Δ, εVal = θ_Δ, settledBuffer = def }
        in (acd', aps')
        where
            θ_Δ = fromIntegral (θ - distributionLimit acd)
            flowBufferDelta = newFlowBuffer - flowBuffer acd

instance SuperfluidDistribution sfd => Applicative (DFAPartiesF sfd) where
    pure a = DFAPartiesF a a
    liftA2 f (DFAPartiesF s r) (DFAPartiesF s' r') = DFAPartiesF (f s s') (f r r')

instance SuperfluidDistribution sfd => TaggedTypeable (DFAContractData sfd) where tagFromProxy _ = "DFA#"

instance SuperfluidDistribution sfd => Default (DFAContractData sfd) where
    def = DFAContractData
        { flowLastUpdatedAt = def
        , decayingFactor = default_lambda
        , distributionLimit = def
        , flowBuffer = def
        }

instance SuperfluidDistribution sfd => TaggedTypeable (DFAAccountData sfd) where tagFromProxy _ = "DFA"
instance SuperfluidDistribution sfd => Default (DFAAccountData sfd) where
    def = DFAAccountData
        { settledAt = def
        , αVal = def
        , εVal = def
        , settledBuffer = def
        }
instance SuperfluidDistribution sfd => Semigroup (DFAAccountData sfd) where
    -- Formula:
    --   aad_mappend(a, b) = DFA_AAD
    --     { t_s = t_s'
    --     , α = α * e ^ (λ * (t_s - t_s')) - ε'
    --     , ε = ε + ε'
    --     }
    --     where { t_s = t_s, αVal = α, εVal = ε } = a
    --           { t_s = t_s', αVal = α', εVal = ε' } = b
    (<>) a b = DFAAccountData
        { settledAt = settledAt b
        , αVal = (αVal a) * exp (-λ * t_Δ) - εVal b
        , εVal = εVal a + εVal b
        , settledBuffer = settledBuffer a + settledBuffer b
        }
        where λ = default_lambda
              t_Δ = fromIntegral (settledAt b - settledAt a)
