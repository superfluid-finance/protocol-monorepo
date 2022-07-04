{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}

module Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement
    ( MintedLiquidity
    , mintedLiquidityTag
    , mkMintedLiquidity
    , AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementParties (..)
    , TBAContractData
    , TBAAccountData
    , TBAParties
    -- operations
    , mintLiquidity
    , burnLiquidity
    , transferLiquidity
    ) where

import           Data.Default                                      (Default (..))
import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))
import           Data.Typeable                                     (Proxy (..))

import           Money.Systems.Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Money.Systems.Superfluid.Concepts.Liquidity
    ( Liquidity
    , TappedLiquidity (..)
    , TappedLiquidityTag
    , TypedLiquidity (..)
    , TypedLiquidityTag
    , UntappedLiquidity (..)
    , mkAnyTappedLiquidity
    )
import           Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( TypedLiquidityVector (..)
    , typedLiquidityVectorToRTB
    )
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))

-- | MintedLiquidity Type
--
data MintedLiquidityTag deriving anyclass (TypedLiquidityTag, TappedLiquidityTag)
instance TaggedTypeable MintedLiquidityTag where tagFromProxy _ = "m"
mintedLiquidityTag :: Proxy MintedLiquidityTag
mintedLiquidityTag = Proxy @MintedLiquidityTag
type MintedLiquidity lq = TappedLiquidity MintedLiquidityTag lq
mkMintedLiquidity :: Liquidity lq => lq -> MintedLiquidity lq
mkMintedLiquidity = TappedLiquidity

type TBA :: Type -> Type
data TBA sft
type TBAContractData sft = AgreementContractData (TBA sft)
type TBAAccountData sft = AgreementAccountData (TBA sft)
type TBAParties sft = AgreementParties (TBA sft)
instance SuperfluidTypes sft => Agreement (TBA sft) where
    type DistributionForAgreement (TBA sft) = sft
    data AgreementContractData (TBA sft) = TBAContractData
    data AgreementAccountData (TBA sft) = TBAAccountData
        { settledAt         :: SFT_TS sft
        , untappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
        , mintedLiquidity   :: MintedLiquidity (SFT_LQ sft)
        }
    data AgreementParties (TBA sft) = TBAParties
        { transferFrom :: TBAAccountData sft
        , transferTo   :: TBAAccountData sft
        }

    providedBalanceByAgreement a _ = typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( untappedLiquidity a )
        [ mkAnyTappedLiquidity $ mintedLiquidity a ]

    createAgreementParties = undefined
    liftAgreementParties2 = undefined

instance (SuperfluidTypes sft) => TaggedTypeable (TBAContractData sft) where tagFromProxy _ = "TBA#"
instance (SuperfluidTypes sft) => Default (TBAContractData sft) where def = TBAContractData

instance (SuperfluidTypes sft) => TaggedTypeable (TBAAccountData sft) where tagFromProxy _ = "TBA"
instance SuperfluidTypes sft => Default (TBAAccountData sft) where
    def = TBAAccountData { settledAt = def, untappedLiquidity = def, mintedLiquidity = def }
instance SuperfluidTypes sft => Semigroup (TBAAccountData sft) where
    (<>) = undefined

instance SuperfluidTypes sft => Monoid (TBAAccountData sft) where mempty = def

-- ============================================================================
-- TBA Operations
--
mintLiquidity :: SuperfluidTypes sft => TBAParties sft -> SFT_LQ sft -> TBAParties sft
mintLiquidity (TBAParties from to) l = TBAParties
    ( from { mintedLiquidity = mapLiquidity (flip (-) l) (mintedLiquidity from) } )
    ( to { untappedLiquidity = mapLiquidity (+ l) (untappedLiquidity to) } )

burnLiquidity :: SuperfluidTypes sft => TBAParties sft -> SFT_LQ sft -> TBAParties sft
burnLiquidity (TBAParties from to) l = TBAParties
    ( from { mintedLiquidity = mapLiquidity (+ l) (mintedLiquidity from) } )
    ( to { untappedLiquidity = mapLiquidity (flip (-) l) (untappedLiquidity to) } )

transferLiquidity :: SuperfluidTypes sft => TBAParties sft -> SFT_LQ sft -> TBAParties sft
transferLiquidity (TBAParties from to) l = TBAParties
    ( from { untappedLiquidity = mapLiquidity (flip (-) l) (untappedLiquidity from) } )
    ( to   { untappedLiquidity = mapLiquidity (+ l) (untappedLiquidity to) })
