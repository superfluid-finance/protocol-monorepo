{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Decaying Flow Agreement (DFA)
--
-- NOTE: The formulas can be obtained from the output of maths/DFA.py
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement
    ( MonetaryUnitLenses(..)
    , MonetaryUnitData (..)
    , ContractLens (..)
    , ContractData (..)
    , AgreementContractPartiesF (..)
    , DistributionLimit
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
class (Default amudL, SuperfluidTypes sft) => MonetaryUnitLenses amudL sft | amudL -> sft where
    decayingFactor  :: Lens' amudL (SFT_FLOAT sft)
    settledAt       :: Lens' amudL (SFT_TS sft)
    αVal            :: Lens' amudL (SFT_FLOAT sft)
    εVal            :: Lens' amudL (SFT_FLOAT sft)
    settledBuffer   :: Lens' amudL (BBS.BufferValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amudL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amudL } deriving (Default)

instance MonetaryUnitLenses amudL sft => Semigroup (MonetaryUnitData amudL sft) where
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
instance MonetaryUnitLenses amudL sft => Monoid (MonetaryUnitData amudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amudL sft => AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft where
    -- | Provided balance by DFA
    --
    -- Formula:
    --   rtb(aad, t) = α * e ^ (-λ * (t - t_s)) + ε
    --       where { t_s = t_s, αVal = α, εVal = ε } = aad
    balanceProvidedByAgreement (MkMonetaryUnitData a) t = typedValuesToRTB
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
    distributionLimit :: Lens' cdL (SFT_MVAL sft)
    flowBuffer        :: Lens' cdL (BBS.BufferValue (SFT_MVAL sft))

type ContractData :: Type -> Type -> Type -> Type
newtype ContractData cdL amudL sft = MkContractData { getContractLenses :: cdL } deriving (Default)

type DistributionLimit sft = SFT_MVAL sft

instance ( ContractLens cdL sft
         , MonetaryUnitLenses amudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft
         ) => AgreementContractData (ContractData cdL amudL sft) (MonetaryUnitData amudL sft) sft where

    data AgreementContractPartiesF (ContractData cdL amudL sft) a = ContractPartiesF
        { decayingFlowSender   :: a
        , decayingFlowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cdL amudL sft) =
        --                 θ/distributionLimit     newFlowBuffer
        UpdateDecayingFlow (DistributionLimit sft) (BBS.BufferValue (SFT_MVAL sft))

    -- | Create data of agreement parties from the changes of the DFA contract
    --
    -- Formula:
    --   aad_mempty_update_with_acd(aad, θ_Δ, t_u) = DFA_AAD { t_s = t_u , α = θ_Δ , ε = -θ_Δ }
    applyAgreementOperation (MkContractData acd) (UpdateDecayingFlow θ newFlowBuffer) t' = let
        acd'  = acd & set distributionLimit θ
                    & set flowBuffer        newFlowBuffer
                    & set flowLastUpdatedAt t'
        acpsΔ = fmap MkMonetaryUnitData (ContractPartiesF
                    (def & set settledAt     t'
                         & set αVal          θ_Δ
                         & set εVal          (-θ_Δ)
                         & set settledBuffer flowBufferDelta)
                    (def & set settledAt     t'
                         & set αVal          (-θ_Δ)
                         & set εVal          θ_Δ))
        in (MkContractData acd', acpsΔ)
        where
            θ_Δ             = fromIntegral (θ - acd^.distributionLimit)
            flowBufferDelta = newFlowBuffer - acd^.flowBuffer

type ContractPartiesF   sft cdL amudL = AgreementContractPartiesF (ContractData cdL amudL sft)
type ContractPartiesMUD sft cdL amudL = ContractPartiesF sft cdL (MonetaryUnitData amudL sft)

instance MonetaryUnitLenses amudL sft => Applicative (ContractPartiesF sft cdL amudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
