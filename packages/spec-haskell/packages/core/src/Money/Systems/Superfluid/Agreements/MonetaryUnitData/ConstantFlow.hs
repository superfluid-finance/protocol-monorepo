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

class (Default amudL, SuperfluidTypes sft) => MonetaryUnitLenses amudL sft | amudL -> sft where
    settledAt            :: Lens' amudL (SFT_TS sft)
    settledUntappedValue :: Lens' amudL (UntappedValue (SFT_MVAL sft))
    settledBufferValue   :: Lens' amudL (BBS.BufferValue (SFT_MVAL sft))
    netFlowRate          :: Lens' amudL (SFT_MVAL sft)

type MonetaryUnitData :: Type -> Type -> Type
newtype MonetaryUnitData amudL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amudL } deriving (Default)

instance MonetaryUnitLenses amudL sft => Semigroup (MonetaryUnitData amudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & set  settledAt            (  b^.settledAt)
                  & over settledUntappedValue (+ b^.settledUntappedValue)
                  & over netFlowRate          (+ b^.netFlowRate)
                  & over settledBufferValue   (+ b^.settledBufferValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amudL sft => Monoid (MonetaryUnitData amudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amudL sft => AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) t = typedValuesToRTB
            ( UntappedValue $ uval_s + fr * fromIntegral (t - t_s) )
            [ mkAnyTappedValue buf_s ]
        where t_s                  = a^.settledAt
              UntappedValue uval_s = a^.settledUntappedValue
              buf_s                = a^.settledBufferValue
              fr                   = a^.netFlowRate
