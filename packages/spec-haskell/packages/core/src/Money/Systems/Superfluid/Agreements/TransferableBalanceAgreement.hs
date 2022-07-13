{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement
    ( MintedLiquidity
    , mintedLiquidityTag
    , mkMintedLiquidity
    , MonetaryUnitLenses (..)
    , MonetaryUnitData (..)
    , ContractLenses
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
import           Data.Type.TaggedTypeable          (TaggedTypeable (..))
import           Data.Typeable                     (Proxy (..), Typeable)
import           Lens.Micro

import           Money.Systems.Superfluid.Concepts

-- * MintedLiquidity Type
--

data MintedLiquidityTag deriving anyclass (TappedValueTag)
instance TaggedTypeable MintedLiquidityTag where tagFromProxy _ = "m"
type MintedLiquidity v = TappedValue MintedLiquidityTag v
mintedLiquidityTag :: Proxy MintedLiquidityTag
mintedLiquidityTag = Proxy @MintedLiquidityTag
mkMintedLiquidity :: Value v => v -> MintedLiquidity v
mkMintedLiquidity = TappedValue

-- * TBA.MonetaryUnitData
--
class (Typeable mud, Default mud, SuperfluidTypes sft) => MonetaryUnitLenses mud sft | mud -> sft where
    untappedLiquidity :: Lens' mud (UntappedValue (SFT_LQ sft))
    mintedLiquidity   :: Lens' mud (MintedLiquidity (SFT_LQ sft))

type MonetaryUnitData :: Type -> Type -> Type -- kind signature is required to make GHC happy
newtype MonetaryUnitData _mud sft = MkMonetaryUnitData _mud

instance MonetaryUnitLenses mud sft => TaggedTypeable (MonetaryUnitData mud sft) where
    tagFromProxy _ = "TBA"

instance MonetaryUnitLenses mud sft => Semigroup (MonetaryUnitData mud sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & over untappedLiquidity (+ b^.untappedLiquidity)
                  & over mintedLiquidity   (+ b^.mintedLiquidity)
        in MkMonetaryUnitData c
instance MonetaryUnitLenses mud sft => Monoid (MonetaryUnitData mud sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLenses mud sft => AgreementMonetaryUnitData (MonetaryUnitData mud sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) _ =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( a^.untappedLiquidity )
        [ mkAnyTappedLiquidity $ a^.mintedLiquidity ]

-- * TBA.ContractData
--

class (Typeable cd, Default cd, SuperfluidTypes sft) => ContractLenses cd sft | cd -> sft

type ContractData :: Type -> Type -> Type -> Type
newtype ContractData _cd mud sft = MkContractData _cd

instance (ContractLenses cd sft, Typeable mud) => TaggedTypeable (ContractData cd mud sft) where
    tagFromProxy _ = "TBA#"

instance ContractLenses cd sft => Default (ContractData cd mud sft) where def = MkContractData def

instance ( ContractLenses cd sft
         , MonetaryUnitLenses mud sft
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
