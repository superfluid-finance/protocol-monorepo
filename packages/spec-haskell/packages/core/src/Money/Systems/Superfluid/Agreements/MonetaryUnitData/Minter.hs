{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.MonetaryUnitData.Minter
    ( MintedValueTag
    , mintedValueTag
    , MintedValue
    , mkMintedValue
    , MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    ) where

import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Data.Proxy                        (Proxy (..))
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts

-- * Minted value type
--

-- TODO use TH: $(defineTappedValue MintedValueTag "m" BufferValue)
data MintedValueTag
instance TypedValueTag MintedValueTag where tappedValueTag _ = "m"
instance TappedValueTag MintedValueTag
type MintedValue v = TappedValue MintedValueTag v
mintedValueTag :: Proxy MintedValueTag
mintedValueTag = Proxy @MintedValueTag
mkMintedValue :: Value v => v -> MintedValue v
mkMintedValue = TappedValue

-- * Monetary unit data
--
class (Default amuL, SuperfluidTypes sft) => MonetaryUnitLenses amuL sft | amuL -> sft where
    untappedValue :: Lens' amuL (UntappedValue (SFT_MVAL sft))
    mintedValue   :: Lens' amuL (MintedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type -- make GHC happy
newtype MonetaryUnitData amuL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amuL } deriving (Default)

instance MonetaryUnitLenses amuL sft => Semigroup (MonetaryUnitData amuL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
                  & over mintedValue   (+ b^.mintedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amuL sft => Monoid (MonetaryUnitData amuL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amuL sft => AgreementMonetaryUnitData (MonetaryUnitData amuL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB
        ( a^.untappedValue )
        [ mkAnyTappedValue $ a^.mintedValue ]
