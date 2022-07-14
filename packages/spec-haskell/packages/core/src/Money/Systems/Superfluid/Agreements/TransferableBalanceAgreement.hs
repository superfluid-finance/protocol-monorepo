{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement
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

-- * TBA.MonetaryUnitData
--
class (Default mud, SuperfluidTypes sft) => MonetaryUnitLens mud sft | mud -> sft where
    untappedLiquidity :: Lens' mud (UntappedValue (SFT_LQ sft))
    mintedLiquidity   :: Lens' mud (MintedLiquidity (SFT_LQ sft))

type MonetaryUnitData :: Type -> Type -> Type -- kind signature is required to make GHC happy
newtype MonetaryUnitData mudL sft = MkMonetaryUnitData mudL

instance MonetaryUnitLens mud sft => Semigroup (MonetaryUnitData mud sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedLiquidity (+ b^.untappedLiquidity)
                  & over mintedLiquidity   (+ b^.mintedLiquidity)
        in MkMonetaryUnitData c
instance MonetaryUnitLens mud sft => Monoid (MonetaryUnitData mud sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens mud sft => AgreementMonetaryUnitData (MonetaryUnitData mud sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( a^.untappedLiquidity )
        [ mkAnyTappedLiquidity $ a^.mintedLiquidity ]

-- * TBA.ContractData
--

class (Default cd, SuperfluidTypes sft) => ContractLens cd sft | cd -> sft

type ContractData :: Type -> Type -> Type -> Type
newtype ContractData cdL mud sft = MkContractData cdL

instance ContractLens cd sft => Default (ContractData cd mud sft) where def = MkContractData def

instance ( ContractLens cd sft
         , MonetaryUnitLens mud sft
         , AgreementMonetaryUnitData (MonetaryUnitData mud sft) sft
         ) => AgreementContractData (ContractData cd mud sft) (MonetaryUnitData mud sft) sft where

    data AgreementContractPartiesF (ContractData cd mud sft) a = ContractPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cd mud sft) =
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

type ContractPartiesF   sft cd mud = AgreementContractPartiesF (ContractData cd mud sft)
type ContractPartiesMUD sft cd mud = ContractPartiesF sft cd (MonetaryUnitData mud sft)

instance Applicative (ContractPartiesF sft cd mud) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')
