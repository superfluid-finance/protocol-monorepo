{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

module Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement
    ( MintedLiquidity
    , mintedLiquidityTag
    , mkMintedLiquidity
    , AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementPartiesF (..)
    , AgreementOperation (..)
    , TBAContractData
    , TBAAccountData
    , TBAPartiesF
    , TBAParties
    ) where

import           Control.Applicative                               (Applicative (..))
import           Data.Coerce                                       (coerce)
import           Data.Default                                      (Default (..))
import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))
import           Data.Typeable                                     (Proxy (..))

import           Money.Systems.Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Money.Systems.Superfluid.Concepts.Liquidity
    ( Liquidity
    , TappedLiquidity (..)
    , TappedLiquidityTag
    , UntappedLiquidity (..)
    , mkAnyTappedLiquidity
    )
import           Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( TypedLiquidityVector (..)
    , typedLiquidityVectorToRTB
    )
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))

-- MintedLiquidity Type
--
data MintedLiquidityTag deriving anyclass (TappedLiquidityTag)
instance TaggedTypeable MintedLiquidityTag where tagFromProxy _ = "m"
type MintedLiquidity lq = TappedLiquidity MintedLiquidityTag lq
mintedLiquidityTag :: Proxy MintedLiquidityTag
mintedLiquidityTag = Proxy @MintedLiquidityTag
mkMintedLiquidity :: Liquidity lq => lq -> MintedLiquidity lq
mkMintedLiquidity = TappedLiquidity

-- The Agreement Definition
--
type TBA :: Type -> Type
data TBA sft
type TBAContractData sft = AgreementContractData (TBA sft)
type TBAAccountData sft = AgreementAccountData (TBA sft)
type TBAPartiesF sft = AgreementPartiesF (TBA sft)
type TBAParties sft = (TBAPartiesF sft) (TBAAccountData sft)
instance SuperfluidTypes sft => Agreement (TBA sft) where
    type DistributionForAgreement (TBA sft) = sft

    data AgreementContractData (TBA sft) = TBAContractData

    data AgreementAccountData (TBA sft) = TBAAccountData
        { untappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
        , mintedLiquidity   :: MintedLiquidity (SFT_LQ sft)
        }

    data AgreementPartiesF (TBA sft) a = TBAPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor)

    data AgreementOperation (TBA sft) =
        MintLiquidity (SFT_LQ sft) |
        BurnLiquidity (SFT_LQ sft) |
        TransferLiquidity (SFT_LQ sft)

    providedBalanceByAgreement a _ = typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( untappedLiquidity a )
        [ mkAnyTappedLiquidity $ mintedLiquidity a ]

    createAgreementPartiesDelta acd (MintLiquidity amount) = let
        acd' = acd
        aps' = TBAPartiesF def { mintedLiquidity = coerce (- amount) }
                           def { untappedLiquidity = coerce amount }
        in (acd', aps')
    createAgreementPartiesDelta acd (BurnLiquidity amount) = let
        acd' = acd
        aps' = TBAPartiesF def { mintedLiquidity = coerce amount }
                           def { untappedLiquidity = coerce (- amount) }
        in (acd', aps')
    createAgreementPartiesDelta acd (TransferLiquidity amount) = let
        acd' = acd
        aps' = TBAPartiesF def { untappedLiquidity = coerce (- amount) }
                           def { untappedLiquidity = coerce amount }
        in (acd', aps')

instance SuperfluidTypes sft => Applicative (TBAPartiesF sft) where
    pure a = TBAPartiesF a a
    liftA2 f (TBAPartiesF s r) (TBAPartiesF s' r') = TBAPartiesF (f s s') (f r r')

instance (SuperfluidTypes sft) => TaggedTypeable (TBAContractData sft) where tagFromProxy _ = "TBA#"
instance (SuperfluidTypes sft) => Default (TBAContractData sft) where def = TBAContractData

instance (SuperfluidTypes sft) => TaggedTypeable (TBAAccountData sft) where tagFromProxy _ = "TBA"
instance SuperfluidTypes sft => Default (TBAAccountData sft) where
    def = TBAAccountData { untappedLiquidity = def, mintedLiquidity = def }
instance SuperfluidTypes sft => Semigroup (TBAAccountData sft) where
    (<>) a b = TBAAccountData
        { untappedLiquidity = untappedLiquidity a + untappedLiquidity b
        , mintedLiquidity = mintedLiquidity a + mintedLiquidity b
        }
