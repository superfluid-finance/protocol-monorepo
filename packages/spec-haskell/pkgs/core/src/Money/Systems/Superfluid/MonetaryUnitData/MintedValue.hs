{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.MonetaryUnitData.MintedValue
    ( MintedValue (..)
    , MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                         (Default (..))
import           Data.Kind                            (Type)
import           Data.Typeable
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes

-- * Minted value type
--

newtype MintedValue v = MkMintedValue v
    deriving newtype (Default, Enum, Num, Eq, Ord, Real, Integral, MonetaryValue)
instance (Typeable v, MonetaryValue v) => TypedValue (MintedValue v) v where typedValueTag _ = "b"

-- * Monetary unit data
--
class (Default amuLs, SuperfluidSystemTypes sft) => MonetaryUnitLenses amuLs sft | amuLs -> sft where
    untappedValue :: Lens' amuLs (UntappedValue (SFT_MVAL sft))
    mintedValue   :: Lens' amuLs (MintedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type -- make GHC happy
newtype MonetaryUnitData amuLs sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuLs }
    deriving (Default)

instance MonetaryUnitLenses amuLs sft => Semigroup (MonetaryUnitData amuLs sft) where
    MkMonetaryUnitData a <> MkMonetaryUnitData b =
        let c = a & over untappedValue (+ b^.untappedValue)
                  & over mintedValue   (+ b^.mintedValue)
        in MkMonetaryUnitData c

instance MonetaryUnitLenses amuLs sft => MonetaryUnitDataClass (MonetaryUnitData amuLs sft) sft where
    balanceProvided (MkMonetaryUnitData a) _ = typedValuesToRTB
        [ mkAnyTypedValue $ a^.untappedValue
        , mkAnyTypedValue $ a^.mintedValue
        ]
