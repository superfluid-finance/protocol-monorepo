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
    , TBAContractData
    , TBAAccountData
    , TBAPartiesF
    , TBAParties
    -- operations
    , mintLiquidity
    , burnLiquidity
    , transferLiquidity
    ) where

import           Control.Applicative                               (Applicative (..))
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

-- The Agrement
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
        { settledAt         :: SFT_TS sft
        , untappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
        , mintedLiquidity   :: MintedLiquidity (SFT_LQ sft)
        }
    data AgreementPartiesF (TBA sft) a = TBAPartiesF { transferFrom :: a
                                                     , transferTo   :: a
                                                     } deriving stock (Functor)

    providedBalanceByAgreement a _ = typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( untappedLiquidity a )
        [ mkAnyTappedLiquidity $ mintedLiquidity a ]

    createAgreementPartiesDelta = undefined

instance SuperfluidTypes sft => Applicative (TBAPartiesF sft) where
    pure a = TBAPartiesF a a
    liftA2 = undefined

instance (SuperfluidTypes sft) => TaggedTypeable (TBAContractData sft) where tagFromProxy _ = "TBA#"
instance (SuperfluidTypes sft) => Default (TBAContractData sft) where def = TBAContractData

instance (SuperfluidTypes sft) => TaggedTypeable (TBAAccountData sft) where tagFromProxy _ = "TBA"
instance SuperfluidTypes sft => Default (TBAAccountData sft) where
    def = TBAAccountData { settledAt = def, untappedLiquidity = def, mintedLiquidity = def }
instance SuperfluidTypes sft => Semigroup (TBAAccountData sft) where
    (<>) = undefined

-- ============================================================================
-- TBA Operations
--
mintLiquidity :: SuperfluidTypes sft => TBAParties sft -> SFT_LQ sft -> TBAParties sft
mintLiquidity (TBAPartiesF from to) l = TBAPartiesF
    ( from { mintedLiquidity = fmap (flip (-) l) (mintedLiquidity from) } )
    ( to { untappedLiquidity = fmap (+ l) (untappedLiquidity to) } )

burnLiquidity :: SuperfluidTypes sft => TBAParties sft -> SFT_LQ sft -> TBAParties sft
burnLiquidity (TBAPartiesF from to) l = TBAPartiesF
    ( from { mintedLiquidity = fmap (+ l) (mintedLiquidity from) } )
    ( to { untappedLiquidity = fmap (flip (-) l) (untappedLiquidity to) } )

transferLiquidity :: SuperfluidTypes sft => TBAParties sft -> SFT_LQ sft -> TBAParties sft
transferLiquidity (TBAPartiesF from to) l = TBAPartiesF
    ( from { untappedLiquidity = fmap (flip (-) l) (untappedLiquidity from) } )
    ( to   { untappedLiquidity = fmap (+ l) (untappedLiquidity to) })
