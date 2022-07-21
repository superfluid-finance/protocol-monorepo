{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.MinterAgreement
    ( MintedValueTag
    , mintedValueTag
    , MintedValue
    , mkMintedValue
    , MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    , ContractData
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractPartiesF
    , ContractPartiesMUD
    ) where

import           Control.Applicative               (Applicative (..))
import           Data.Coerce                       (coerce)
import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Data.Proxy                        (Proxy (..))
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts

-- * MintedValue Type
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

-- * MINTA.MonetaryUnitData
--
class (Default amudL, SuperfluidTypes sft) => MonetaryUnitLenses amudL sft | amudL -> sft where
    untappedValue :: Lens' amudL (UntappedValue (SFT_MVAL sft))
    mintedValue   :: Lens' amudL (MintedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type -- make GHC happy
newtype MonetaryUnitData amudL sft = MkMonetaryUnitData { getMonetaryUnitLenses :: amudL } deriving (Default)

instance MonetaryUnitLenses amudL sft => Semigroup (MonetaryUnitData amudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
                  & over mintedValue   (+ b^.mintedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses amudL sft => Monoid (MonetaryUnitData amudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses amudL sft => AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB
        ( a^.untappedValue )
        [ mkAnyTappedValue $ a^.mintedValue ]

-- * MINTA.ContractData
--

-- No ongoing contract
type ContractData :: Type -> Type -> Type
data ContractData amudL sft = ContractData
instance Default (ContractData amudL sft) where def = ContractData

instance ( MonetaryUnitLenses amudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft
         ) => AgreementContractData (ContractData amudL sft) (MonetaryUnitData amudL sft) sft where

    data AgreementContractPartiesF (ContractData amudL sft) a = ContractPartiesF
        { mintFrom :: a
        , mintTo   :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData amudL sft) =
        Mint (SFT_MVAL sft) |
        Burn (SFT_MVAL sft)

    applyAgreementOperation acd (Mint amount) _ = let
        acd'  = acd
        acpsΔ = fmap MkMonetaryUnitData (ContractPartiesF
                    (def & mintedValue   .~ coerce (- amount))
                    (def & untappedValue .~ coerce    amount))
        in (acd', acpsΔ)
    applyAgreementOperation acd (Burn amount) _ = let
        acd'  = acd
        acpsΔ = fmap MkMonetaryUnitData (ContractPartiesF
                    (def & mintedValue   .~ coerce    amount)
                    (def & untappedValue .~ coerce (- amount)))
        in (acd', acpsΔ)

type ContractPartiesF   sft amudL = AgreementContractPartiesF (ContractData amudL sft)
type ContractPartiesMUD sft amudL = ContractPartiesF sft (MonetaryUnitData amudL sft)

instance Applicative (ContractPartiesF sft amudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
