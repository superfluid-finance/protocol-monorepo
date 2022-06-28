{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement
    ( MintedLiquidity
    , mintedLiquidityTag
    , mkMintedLiquidity
    , TBAAccountData (..)
    , TransferPair (..)
    , mintLiquidity
    , burnLiquidity
    , transferLiquidity
    ) where

import           Data.Default                                      (Default (..))
import           Data.Kind                                         (Type)
import           Data.Typeable                                     (Proxy (..))
import           Text.Printf                                       (printf)

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
--
import           Data.Internal.TaggedTypeable


-- | MintedLiquidity Type
--
data MintedLiquidityTag deriving anyclass (TypedLiquidityTag, TappedLiquidityTag)
instance TaggedTypeable MintedLiquidityTag where tagFromProxy _ = "m"
mintedLiquidityTag :: Proxy MintedLiquidityTag
mintedLiquidityTag = Proxy @MintedLiquidityTag
type MintedLiquidity lq = TappedLiquidity MintedLiquidityTag lq
mkMintedLiquidity :: Liquidity lq => lq -> MintedLiquidity lq
mkMintedLiquidity = TappedLiquidity

-- | TBAAccountData Type (is AgreementAccountData)
--
type TBAAccountData:: Type -> Type
data TBAAccountData sft = TBAAccountData
    { settledAt         :: SFT_TS sft
    , untappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
    , mintedLiquidity   :: MintedLiquidity (SFT_LQ sft)
    }
instance (SuperfluidTypes sft) => TaggedTypeable (TBAAccountData sft) where tagFromProxy _ = "TBA"

instance (SuperfluidTypes sft) => AgreementAccountData (TBAAccountData sft) sft where
    providedBalanceOfAgreement a _ = typedLiquidityVectorToRTB $ TypedLiquidityVector
        ( untappedLiquidity a )
        [ mkAnyTappedLiquidity $ mintedLiquidity a ]

instance SuperfluidTypes sft => Show (TBAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s, mliq = %s }"
        (show $ settledAt x)
        (show $ untappedLiquidity x)
        (show $ mintedLiquidity x)

instance SuperfluidTypes sft => Semigroup (TBAAccountData sft) where
    (<>) = undefined
instance SuperfluidTypes sft => Monoid (TBAAccountData sft) where
    mempty = TBAAccountData { settledAt = def, untappedLiquidity = def, mintedLiquidity = def }

-- ============================================================================
-- TBA Operations
--
data TransferPair sft = TransferPair
    { transferFrom :: TBAAccountData sft
    , transferTo   :: TBAAccountData sft
    }

mintLiquidity :: SuperfluidTypes sft => TransferPair sft -> SFT_LQ sft -> TransferPair sft
mintLiquidity (TransferPair from to) l = TransferPair
    ( from { mintedLiquidity = mapLiquidity (flip (-) l) (mintedLiquidity from) } )
    ( to { untappedLiquidity = mapLiquidity (+ l) (untappedLiquidity to) } )

burnLiquidity :: SuperfluidTypes sft => TransferPair sft -> SFT_LQ sft -> TransferPair sft
burnLiquidity (TransferPair from to) l = TransferPair
    ( from { mintedLiquidity = mapLiquidity (+ l) (mintedLiquidity from) } )
    ( to { untappedLiquidity = mapLiquidity (flip (-) l) (untappedLiquidity to) } )

transferLiquidity :: SuperfluidTypes sft => TransferPair sft -> SFT_LQ sft -> TransferPair sft
transferLiquidity (TransferPair from to) l = TransferPair
    ( from { untappedLiquidity = mapLiquidity (flip (-) l) (untappedLiquidity from) } )
    ( to   { untappedLiquidity = mapLiquidity (+ l) (untappedLiquidity to) })
