{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

module Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement
    ( MintedLiquidity
    , mintedLiquidityTag
    , mkMintedLiquidity
    , AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractData
    , AccountData
    , ContractPartiesF
    , ContractParties
    ) where

import           Control.Applicative               (Applicative (..))
import           Data.Coerce                       (coerce)
import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Data.Type.TaggedTypeable          (TaggedTypeable (..))
import           Data.Typeable                     (Proxy (..))

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

-- * Agreement Definition
--

type TBA :: Type -> Type -- kind signature is required to make GHC happy
data TBA sft

instance SuperfluidTypes sft => Agreement (TBA sft) sft where
    data AgreementContractData (TBA sft) = ContractData

    data AgreementAccountData (TBA sft) = AccountData
        { untappedLiquidity :: UntappedValue (SFT_LQ sft)
        , mintedLiquidity   :: MintedLiquidity (SFT_LQ sft)
        }

    -- FIXME this is not contract data
    data AgreementContractPartiesF (TBA sft) a = ContractPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (TBA sft) =
        MintLiquidity (SFT_LQ sft) |
        BurnLiquidity (SFT_LQ sft) |
        TransferLiquidity (SFT_LQ sft)

    balanceProvidedByAgreement a _ = typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( untappedLiquidity a )
        [ mkAnyTappedLiquidity $ mintedLiquidity a ]

    applyAgreementOperation acd (MintLiquidity amount) = let
        acd' = acd
        acps' = ContractPartiesF def { mintedLiquidity   = coerce (- amount) }
                                 def { untappedLiquidity = coerce amount     }
        in (acd', acps')
    applyAgreementOperation acd (BurnLiquidity amount) = let
        acd' = acd
        acps' = ContractPartiesF def { mintedLiquidity   = coerce amount     }
                                 def { untappedLiquidity = coerce (- amount) }
        in (acd', acps')
    applyAgreementOperation acd (TransferLiquidity amount) = let
        acd' = acd
        acps' = ContractPartiesF def { untappedLiquidity = coerce (- amount) }
                                 def { untappedLiquidity = coerce amount     }
        in (acd', acps')

type ContractData sft = AgreementContractData (TBA sft)
type AccountData sft = AgreementAccountData (TBA sft)
type ContractPartiesF sft = AgreementContractPartiesF (TBA sft)
type ContractParties sft = (ContractPartiesF sft) (AccountData sft)

instance SuperfluidTypes sft => Applicative (ContractPartiesF sft) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')

instance (SuperfluidTypes sft) => TaggedTypeable (ContractData sft) where tagFromProxy _ = "TBA#"
instance (SuperfluidTypes sft) => Default (ContractData sft) where def = ContractData

instance (SuperfluidTypes sft) => TaggedTypeable (AccountData sft) where tagFromProxy _ = "TBA"
instance SuperfluidTypes sft => Default (AccountData sft) where
    def = AccountData { untappedLiquidity = def, mintedLiquidity = def }
instance SuperfluidTypes sft => Semigroup (AccountData sft) where
    (<>) a b = AccountData
        { untappedLiquidity = untappedLiquidity a + untappedLiquidity b
        , mintedLiquidity = mintedLiquidity a + mintedLiquidity b
        }
