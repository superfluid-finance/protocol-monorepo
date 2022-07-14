{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Decaying Flow Agreement (DFA)
--
-- NOTE: The formulas can be obtained from the output of maths/DFA.py
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement
    ( MonetaryUnitLens(..)
    , MonetaryUnitData (..)
    , ContractLens (..)
    , ContractData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractPartiesF
    , ContractPartiesMUD
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- * DFA.MonetaryUnitData
--
class (Default mudL, SuperfluidTypes sft) => MonetaryUnitLens mudL sft | mudL -> sft where
    decayingFactor  :: Lens' mudL (SFT_FLOAT sft)
    settledAt       :: Lens' mudL (SFT_TS sft)
    αVal            :: Lens' mudL (SFT_FLOAT sft)
    εVal            :: Lens' mudL (SFT_FLOAT sft)
    settledBuffer   :: Lens' mudL (BBS.BufferValue (SFT_LQ sft))

type MonetaryUnitData :: Type -> Type -> Type -- kind signature is required to make GHC happy
newtype MonetaryUnitData mudL sft = MkMonetaryUnitData mudL

instance MonetaryUnitLens mudL sft => Semigroup (MonetaryUnitData mudL sft) where
    -- Formula:
    --   aad_mappend(a, b) = DFA_AAD
    --     { t_s = t_s'
    --     , α   = α * e ^ (λ * (t_s - t_s')) - ε'
    --     , ε   = ε + ε'
    --     }
    --     where { t_s = t_s,  αVal = α,  εVal = ε  } = a
    --           { t_s = t_s', αVal = α', εVal = ε' } = b
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & set  settledAt     (b^.settledAt)
                  & over αVal          (\α -> α * exp (-λ * t_Δ) - ε')
                  & over εVal          (+ ε')
                  & over settledBuffer (+ b^.settledBuffer)
        in MkMonetaryUnitData c
        where ε'  = b^.εVal
              λ   = b^.decayingFactor
              t_Δ = fromIntegral (b^.settledAt - a^.settledAt)
instance MonetaryUnitLens mudL sft => Monoid (MonetaryUnitData mudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens mudL sft => AgreementMonetaryUnitData (MonetaryUnitData mudL sft) sft where
    -- | Provided balance by DFA
    --
    -- Formula:
    --   rtb(aad, t) = α * e ^ (-λ * (t - t_s)) + ε
    --       where { t_s = t_s, αVal = α, εVal = ε } = aad
    balanceProvidedByAgreement (MkMonetaryUnitData a) t =
        typedValueVectorToRTB $ TypedValueVector
            ( UntappedValue $ ceiling $ α * exp (-λ * t_Δ) + ε )
            [ mkAnyTappedValue buf_s ]
        where t_s   = a^.settledAt
              α     = a^.αVal
              ε     = a^.εVal
              buf_s = a^.settledBuffer
              λ     = a^.decayingFactor
              t_Δ   = fromIntegral (t - t_s)

-- * DFA.ContractData
--

class (Default cdL, SuperfluidTypes sft) => ContractLens cdL sft | cdL -> sft where
    flowLastUpdatedAt :: Lens' cdL (SFT_TS sft)
    distributionLimit :: Lens' cdL (SFT_LQ sft)
    flowBuffer        :: Lens' cdL (BBS.BufferValue (SFT_LQ sft))

type ContractData :: Type -> Type -> Type -> Type
data ContractData cdL mudL sft = MkContractData cdL

instance ContractLens cdL sft => Default (ContractData cdL mudL sft) where def = MkContractData def

instance ( ContractLens cdL sft
         , MonetaryUnitLens mudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData mudL sft) sft
         ) => AgreementContractData (ContractData cdL mudL sft) (MonetaryUnitData mudL sft) sft where

    data AgreementContractPartiesF (ContractData cdL mudL sft) a = ContractPartiesF
        { decayingFlowSender   :: a
        , decayingFlowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cdL mudL sft) =
        -- θ (Distribution Limit), newFlowBuffer, t'
        UpdateDecayingFlow (SFT_LQ sft) (BBS.BufferValue (SFT_LQ sft)) (SFT_TS sft)

    -- | Create data of agreement parties from the changes of the DFA contract
    --
    -- Formula:
    --   aad_mempty_update_with_acd(aad, θ_Δ, t_u) = DFA_AAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation (MkContractData acd) acps (UpdateDecayingFlow θ newFlowBuffer t') = let
        acd' = acd & set distributionLimit θ
                   & set flowBuffer        newFlowBuffer
                   & set flowLastUpdatedAt t'
        acps' = (<>) <$> acps <*> fmap MkMonetaryUnitData (ContractPartiesF
                    (def & set settledAt     t'
                         & set αVal          θ_Δ
                         & set εVal          (-θ_Δ)
                         & set settledBuffer flowBufferDelta)
                    (def & set settledAt     t'
                         & set αVal          (-θ_Δ)
                         & set εVal          θ_Δ))
        in (MkContractData acd', acps')
        where
            θ_Δ             = fromIntegral (θ - acd^.distributionLimit)
            flowBufferDelta = newFlowBuffer - acd^.flowBuffer

type ContractPartiesF   sft cdL mudL = AgreementContractPartiesF (ContractData cdL mudL sft)
type ContractPartiesMUD sft cdL mudL = ContractPartiesF sft cdL (MonetaryUnitData mudL sft)

instance MonetaryUnitLens mudL sft => Applicative (ContractPartiesF sft cdL mudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
