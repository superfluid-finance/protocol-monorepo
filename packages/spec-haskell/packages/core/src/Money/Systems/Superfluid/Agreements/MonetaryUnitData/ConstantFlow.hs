{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow
    ( MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS

class (Default amuLs, SuperfluidTypes sft) => MonetaryUnitLenses amuLs sft | amuLs -> sft where
    settledAt          :: Lens' amuLs (SFT_TS sft)
    settledValue       :: Lens' amuLs (UntappedValue (SFT_MVAL sft))
    netFlowRate        :: Lens' amuLs (SFT_MVAL sft)
    settledBufferValue :: Lens' amuLs (BBS.BufferValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amuLs sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuLs } deriving (Default)

instance MonetaryUnitLenses amuLs sft => Semigroup (MonetaryUnitData amuLs sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let t  = a^.settledAt
            t' = b^.settledAt
            settledΔ = UntappedValue $ a^.netFlowRate * fromIntegral (t' - t)
            c = a & set  settledAt t'
                  & over netFlowRate        (+ b^.netFlowRate)
                  & over settledValue       (+ (b^.settledValue + settledΔ))
                  & over settledBufferValue (+ b^.settledBufferValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amuLs sft => Monoid (MonetaryUnitData amuLs sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amuLs sft => AgreementMonetaryUnitData (MonetaryUnitData amuLs sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) t = typedValuesToRTB
            ( UntappedValue $ uval_s + fr * fromIntegral (t - t_s) )
            [ mkAnyTappedValue buf_s ]
        where t_s                  = a^.settledAt
              UntappedValue uval_s = a^.settledValue
              buf_s                = a^.settledBufferValue
              fr                   = a^.netFlowRate
