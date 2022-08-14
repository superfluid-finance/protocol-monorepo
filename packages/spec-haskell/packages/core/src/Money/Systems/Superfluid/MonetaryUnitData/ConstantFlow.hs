{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow
    ( MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                         (Default (..))
import           Data.Kind                            (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes

class (Default amuLs, SuperfluidSystemTypes sft) => MonetaryUnitLenses amuLs sft | amuLs -> sft where
    settledAt          :: Lens' amuLs (SFT_TS sft)
    settledValue       :: Lens' amuLs (UntappedValue (SFT_MVAL sft))
    netFlowRate        :: Lens' amuLs (SFT_MVAL sft)

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
        in MkMonetaryUnitData c

instance MonetaryUnitLenses amuLs sft => MonetaryUnitDataClass (MonetaryUnitData amuLs sft) sft where
    balanceProvided (MkMonetaryUnitData a) t = typedValuesToRTB
            ( UntappedValue $ uval_s + fr * fromIntegral (t - t_s) )
            [ ]
        where t_s                  = a^.settledAt
              UntappedValue uval_s = a^.settledValue
              fr                   = a^.netFlowRate
