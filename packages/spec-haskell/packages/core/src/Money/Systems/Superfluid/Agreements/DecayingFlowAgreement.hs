{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Decaying Flow Agreement (DFA)
--
-- NOTE: The formulas can be obtained from the output of maths/DFA.py
module Money.Systems.Superfluid.Agreements.DecayingFlowAgreement
    ( MonetaryUnitLenses(..)
    , MonetaryUnitData (..)
    , ContractLenses (..)
    , ContractData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractPartiesF
    , ContractPartiesMUD
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Data.Type.TaggedTypeable
import           Data.Typeable                                           (Typeable)
import           Lens.Micro

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


default_lambda :: RealFloat b => b
default_lambda = log 2 / (3600 * 24 * 7)

-- * DFA.MonetaryUnitData
--
class (Typeable mud, Default mud, SuperfluidTypes sft) => MonetaryUnitLenses mud sft | mud -> sft where
    decayingFactor  :: Lens' mud (SFT_FLOAT sft)
    settledAt       :: Lens' mud (SFT_TS sft)
    αVal            :: Lens' mud (SFT_FLOAT sft)
    εVal            :: Lens' mud (SFT_FLOAT sft)
    settledBuffer   :: Lens' mud (BBS.BufferLiquidity (SFT_LQ sft))

type MonetaryUnitData :: Type -> Type -> Type -- kind signature is required to make GHC happy
newtype MonetaryUnitData _mud sft = MkMonetaryUnitData _mud
instance MonetaryUnitLenses mud sft => TaggedTypeable (MonetaryUnitData mud sft) where
    tagFromProxy _ = "DFA"

instance MonetaryUnitLenses mud sft => Semigroup (MonetaryUnitData mud sft) where
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
              λ   = default_lambda
              t_Δ = fromIntegral (b^.settledAt - a^.settledAt)
instance MonetaryUnitLenses mud sft => Monoid (MonetaryUnitData mud sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses mud sft => AgreementMonetaryUnitData (MonetaryUnitData mud sft) sft where
    -- | Provided balance by DFA
    --
    -- Formula:
    --   rtb(aad, t) = α * e ^ (-λ * (t - t_s)) + ε
    --       where { t_s = t_s, αVal = α, εVal = ε } = aad
    balanceProvidedByAgreement (MkMonetaryUnitData a) t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedValue $ ceiling $ α * exp (-λ * t_Δ) + ε )
            [ mkAnyTappedLiquidity buf_s ]
        where t_s   = a^.settledAt
              α     = a^.αVal
              ε     = a^.εVal
              buf_s = a^.settledBuffer
              λ     = default_lambda
              t_Δ   = fromIntegral (t - t_s)

-- * DFA.ContractData
--

class (Typeable cd, Default cd, SuperfluidTypes sft) => ContractLenses cd sft | cd -> sft where
    flowLastUpdatedAt :: Lens' cd (SFT_TS sft)
    distributionLimit :: Lens' cd (SFT_LQ sft)
    flowBuffer        :: Lens' cd (BBS.BufferLiquidity (SFT_LQ sft))

type ContractData :: Type -> Type -> Type -> Type
data ContractData _cd mud sft = MkContractData _cd
instance (ContractLenses cd sft, Typeable mud) => TaggedTypeable (ContractData cd mud sft) where
    tagFromProxy _ = "DFA#"
instance ContractLenses cd sft => Default (ContractData cd mud sft) where def = MkContractData def

instance ( ContractLenses cd sft
         , MonetaryUnitLenses mud sft
         , AgreementMonetaryUnitData (MonetaryUnitData mud sft) sft
         ) => AgreementContractData (ContractData cd mud sft) (MonetaryUnitData mud sft) sft where

    data AgreementContractPartiesF (ContractData cd mud sft) a = ContractPartiesF
        { decayingFlowSender   :: a
        , decayingFlowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cd mud sft) =
        -- θ (Distribution Limit), newFlowBuffer, t'
        UpdateDecayingFlow (SFT_LQ sft) (BBS.BufferLiquidity (SFT_LQ sft)) (SFT_TS sft)

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

type ContractPartiesF   sft cd mud = AgreementContractPartiesF (ContractData cd mud sft)
type ContractPartiesMUD sft cd mud = ContractPartiesF sft cd (MonetaryUnitData mud sft)

instance MonetaryUnitLenses mud sft => Applicative (ContractPartiesF sft cd mud) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
