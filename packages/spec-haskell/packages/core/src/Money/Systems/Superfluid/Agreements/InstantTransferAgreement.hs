{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.InstantTransferAgreement
    ( MintedLiquidity
    , mintedLiquidityTag
    , mkMintedLiquidity
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

-- * MintedLiquidity Type
--

data MintedLiquidityTag
instance TappedValueTag MintedLiquidityTag where tappedValueTag _ = "m"
type MintedLiquidity v = TappedValue MintedLiquidityTag v
mintedLiquidityTag :: Proxy MintedLiquidityTag
mintedLiquidityTag = Proxy @MintedLiquidityTag
mkMintedLiquidity :: Value v => v -> MintedLiquidity v
mkMintedLiquidity = TappedValue

-- * ITA.MonetaryUnitData
--
class (Default mudL, SuperfluidTypes sft) => MonetaryUnitLens mudL sft | mudL -> sft where
    untappedLiquidity :: Lens' mudL (UntappedValue (SFT_LQ sft))
    mintedLiquidity   :: Lens' mudL (MintedLiquidity (SFT_LQ sft))

type MonetaryUnitData :: Type -> Type -> Type -- make GHC happy
newtype MonetaryUnitData mudL sft = MkMonetaryUnitData mudL

instance MonetaryUnitLens mudL sft => Semigroup (MonetaryUnitData mudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedLiquidity (+ b^.untappedLiquidity)
                  & over mintedLiquidity   (+ b^.mintedLiquidity)
        in MkMonetaryUnitData c
instance MonetaryUnitLens mudL sft => Monoid (MonetaryUnitData mudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens mudL sft => AgreementMonetaryUnitData (MonetaryUnitData mudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( a^.untappedLiquidity )
        [ mkAnyTappedLiquidity $ a^.mintedLiquidity ]

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
        MintLiquidity (SFT_LQ sft) |
        BurnLiquidity (SFT_LQ sft) |
        TransferLiquidity (SFT_LQ sft)

    applyAgreementOperation acd acps (MintLiquidity amount) = let
        acd'  = acd
        acps' = (<>) <$> acps <*> fmap MkMonetaryUnitData (ContractPartiesF
                    (def & mintedLiquidity   .~ coerce (- amount))
                    (def & untappedLiquidity .~ coerce    amount))
        in (acd', acps')
    applyAgreementOperation acd acps (BurnLiquidity amount) = let
        acd' = acd
        acps' = (<>) <$> acps <*> fmap MkMonetaryUnitData (ContractPartiesF
                    (def & mintedLiquidity   .~ coerce    amount)
                    (def & untappedLiquidity .~ coerce (- amount)))
        in (acd', acps')
    applyAgreementOperation acd acps (TransferLiquidity amount) = let
        acd' = acd
        acps' = (<>) <$> acps <*> fmap MkMonetaryUnitData (ContractPartiesF
                    (def & untappedLiquidity .~ coerce (- amount))
                    (def & untappedLiquidity .~ coerce    amount))
        in (acd', acps')

type ContractPartiesF   sft cdL mudL = AgreementContractPartiesF (ContractData cdL mudL sft)
type ContractPartiesMUD sft cdL mudL = ContractPartiesF sft cdL (MonetaryUnitData mudL sft)

instance Applicative (ContractPartiesF sft cdL mudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
