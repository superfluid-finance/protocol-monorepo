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

import           Control.Applicative               (Applicative (..))
import           Data.Coerce                       (coerce)
import           Data.Default                      (Default (..))
import           Data.Kind                         (Type)
import           Data.Type.TaggedTypeable          (TaggedTypeable (..))
import           Data.Typeable                     (Proxy (..))

import           Money.Systems.Superfluid.Concepts

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
data TBA sfd
type TBAContractData sfd = AgreementContractData (TBA sfd)
type TBAAccountData sfd = AgreementAccountData (TBA sfd)
type TBAPartiesF sfd = AgreementPartiesF (TBA sfd)
type TBAParties sfd = (TBAPartiesF sfd) (TBAAccountData sfd)
instance SuperfluidDistribution sfd => Agreement (TBA sfd) where
    type DistributionForAgreement (TBA sfd) = sfd

    data AgreementContractData (TBA sfd) = TBAContractData

    data AgreementAccountData (TBA sfd) = TBAAccountData
        { untappedLiquidity :: UntappedLiquidity (SFT_LQ sfd)
        , mintedLiquidity   :: MintedLiquidity (SFT_LQ sfd)
        }

    data AgreementPartiesF (TBA sfd) a = TBAPartiesF
        { transferFrom :: a
        , transferTo   :: a
        } deriving stock (Functor)

    data AgreementOperation (TBA sfd) =
        MintLiquidity (SFT_LQ sfd) |
        BurnLiquidity (SFT_LQ sfd) |
        TransferLiquidity (SFT_LQ sfd)

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

instance SuperfluidDistribution sfd => Applicative (TBAPartiesF sfd) where
    pure a = TBAPartiesF a a
    liftA2 f (TBAPartiesF s r) (TBAPartiesF s' r') = TBAPartiesF (f s s') (f r r')

instance (SuperfluidDistribution sfd) => TaggedTypeable (TBAContractData sfd) where tagFromProxy _ = "TBA#"
instance (SuperfluidDistribution sfd) => Default (TBAContractData sfd) where def = TBAContractData

instance (SuperfluidDistribution sfd) => TaggedTypeable (TBAAccountData sfd) where tagFromProxy _ = "TBA"
instance SuperfluidDistribution sfd => Default (TBAAccountData sfd) where
    def = TBAAccountData { untappedLiquidity = def, mintedLiquidity = def }
instance SuperfluidDistribution sfd => Semigroup (TBAAccountData sfd) where
    (<>) a b = TBAAccountData
        { untappedLiquidity = untappedLiquidity a + untappedLiquidity b
        , mintedLiquidity = mintedLiquidity a + mintedLiquidity b
        }
