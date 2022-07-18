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
class (Default mudL, SuperfluidTypes sft) => MonetaryUnitLens mudL sft | mudL -> sft where
    untappedValue :: Lens' mudL (UntappedValue (SFT_MVAL sft))
    mintedValue   :: Lens' mudL (MintedValue (SFT_MVAL sft))

type MonetaryUnitData :: Type -> Type -> Type -- make GHC happy
newtype MonetaryUnitData mudL sft = MkMonetaryUnitData mudL

instance MonetaryUnitLens mudL sft => Semigroup (MonetaryUnitData mudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedValue (+ b^.untappedValue)
                  & over mintedValue   (+ b^.mintedValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLens mudL sft => Monoid (MonetaryUnitData mudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens mudL sft => AgreementMonetaryUnitData (MonetaryUnitData mudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ = typedValuesToRTB
        ( a^.untappedValue )
        [ mkAnyTappedValue $ a^.mintedValue ]

-- * ITA.ContractData
--

class (Default cdL, SuperfluidTypes sft) => ContractLens cdL sft | cdL -> sft

type ContractData :: Type -> Type -> Type -> Type -- make GHC happy
newtype ContractData cdL mudL sft = MkContractData cdL

instance ContractLens cdL sft => Default (ContractData cdL mudL sft) where def = MkContractData def

instance ( ContractLens cdL sft
         , MonetaryUnitLens mudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData mudL sft) sft
         ) => AgreementContractData (ContractData cdL mudL sft) (MonetaryUnitData mudL sft) sft where

    data AgreementContractPartiesF (ContractData cdL mudL sft) a = ContractPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cdL mudL sft) =
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

type ContractPartiesF   sft cdL mudL = AgreementContractPartiesF (ContractData cdL mudL sft)
type ContractPartiesMUD sft cdL mudL = ContractPartiesF sft cdL (MonetaryUnitData mudL sft)

instance Applicative (ContractPartiesF sft cdL mudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
