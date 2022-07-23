{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Decaying Flow Agreement (DFA)
--
-- NOTE: The formulas can be obtained from the output of maths/DFA.py
module Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow
    ( MonetaryUnitLenses(..)
    , MonetaryUnitData (..)
    , AgreementOperation (..)
    ) where

import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS

class (Default amuL, SuperfluidTypes sft) => MonetaryUnitLenses amuL sft | amuL -> sft where
    decayingFactor  :: Lens' amuL (SFT_FLOAT sft)
    settledAt       :: Lens' amuL (SFT_TS sft)
    αVal            :: Lens' amuL (SFT_FLOAT sft)
    εVal            :: Lens' amuL (SFT_FLOAT sft)
    settledBuffer   :: Lens' amuL (BBS.BufferValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amuL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuL } deriving (Default)

instance MonetaryUnitLenses amuL sft => Semigroup (MonetaryUnitData amuL sft) where
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
instance MonetaryUnitLenses amuL sft => Monoid (MonetaryUnitData amuL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amuL sft => AgreementMonetaryUnitData (MonetaryUnitData amuL sft) sft where
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
