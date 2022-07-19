{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.InstantTransferAgreement
    ( MintedValueTag
    , mintedValueTag
    , MintedValue
    , mkMintedValue
    , MonetaryUnitLens (..)
    , MonetaryUnitData (..)
    , ContractLens
    , ContractData (..)
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
    ( AgreementContractData (..)
    , AgreementMonetaryUnitData (..)
    , RealTimeBalance (typedValuesToRTB)
    , SuperfluidTypes (SFT_MVAL)
    , TappedValue (..)
    , TypedValueTag (..)
    , UntappedValue (..)
    , Value
    , mkAnyTappedValue
    )

-- * MintedValue Type
--

data MintedValueTag
instance TypedValueTag MintedValueTag where tappedValueTag _ = "m"
type MintedValue v = TappedValue MintedValueTag v
mintedValueTag :: Proxy MintedValueTag
mintedValueTag = Proxy @MintedValueTag
mkMintedValue :: Value v => v -> MintedValue v
mkMintedValue = TappedValue

-- * ITA.MonetaryUnitData
--
class (Default amudL, SuperfluidTypes sft) => MonetaryUnitLens amudL sft | amudL -> sft where
    untappedValue :: Lens' amudL (UntappedValue (SFT_MVAL sft))
    mintedValue   :: Lens' amudL (MintedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type -- make GHC happy
newtype MonetaryUnitData amudL sft = MkMonetaryUnitData amudL

instance MonetaryUnitLens amudL sft => Semigroup (MonetaryUnitData amudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
                  & over mintedValue   (+ b^.mintedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLens amudL sft => Monoid (MonetaryUnitData amudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens amudL sft => AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB
        ( a^.untappedValue )
        [ mkAnyTappedValue $ a^.mintedValue ]

-- * ITA.ContractData
--

class (Default cdL, SuperfluidTypes sft) => ContractLens cdL sft | cdL -> sft

type ContractData :: Type -> Type -> Type -> Type -- make GHC happy
newtype ContractData cdL amudL sft = MkContractData cdL deriving (Default)

instance ( ContractLens cdL sft
         , MonetaryUnitLens amudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft
         ) => AgreementContractData (ContractData cdL amudL sft) (MonetaryUnitData amudL sft) sft where

    data AgreementContractPartiesF (ContractData cdL amudL sft) a = ContractPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cdL amudL sft) =
        Mint (SFT_MVAL sft) |
        Burn (SFT_MVAL sft) |
        Transfer (SFT_MVAL sft)

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
    applyAgreementOperation acd (Transfer amount) _ = let
        acd'  = acd
        acpsΔ = fmap MkMonetaryUnitData (ContractPartiesF
                    (def & untappedValue .~ coerce (- amount))
                    (def & untappedValue .~ coerce    amount))
        in (acd', acpsΔ)

type ContractPartiesF   sft cdL amudL = AgreementContractPartiesF (ContractData cdL amudL sft)
type ContractPartiesMUD sft cdL amudL = ContractPartiesF sft cdL (MonetaryUnitData amudL sft)

instance Applicative (ContractPartiesF sft cdL amudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
