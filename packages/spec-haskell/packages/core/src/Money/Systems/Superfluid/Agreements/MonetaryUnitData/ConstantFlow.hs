{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

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

class (Default amuL, SuperfluidTypes sft) => MonetaryUnitLenses amuL sft | amuL -> sft where
    settledAt            :: Lens' amuL (SFT_TS sft)
    settledUntappedValue :: Lens' amuL (UntappedValue (SFT_MVAL sft))
    settledBufferValue   :: Lens' amuL (BBS.BufferValue (SFT_MVAL sft))
    netFlowRate          :: Lens' amuL (SFT_MVAL sft)

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amuL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuL } deriving (Default)

instance MonetaryUnitLenses amuL sft => Semigroup (MonetaryUnitData amuL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & set  settledAt            (  b^.settledAt)
                  & over settledUntappedValue (+ b^.settledUntappedValue)
                  & over netFlowRate          (+ b^.netFlowRate)
                  & over settledBufferValue   (+ b^.settledBufferValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amuL sft => Monoid (MonetaryUnitData amuL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amuL sft => AgreementMonetaryUnitData (MonetaryUnitData amuL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) t = typedValuesToRTB
            ( UntappedValue $ uval_s + fr * fromIntegral (t - t_s) )
            [ mkAnyTappedValue buf_s ]
        where t_s                  = a^.settledAt
              UntappedValue uval_s = a^.settledUntappedValue
              buf_s                = a^.settledBufferValue
              fr                   = a^.netFlowRate
